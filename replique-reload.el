;;; replique-reload.el --- Reloading the whole application  -*- lexical-binding: t; -*-

;; Copyright © 2016 Ewen Grosjean

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; One key for an application that has just changed underneath the process.
;;
;; THE CASE THIS IS FOR IS A BRANCH SWITCHED, and it is not the case the
;; other reloads are for.  Those are about what you have just been editing:
;; `replique-reload-all' loads what changed in the language of the buffer you
;; are in, and `replique-reload-css' builds and reloads the stylesheet you
;; are looking at.  Between the two of them, bringing a running application
;; up to date after a checkout is three keys pressed in three buffers, one of
;; which you have to go and find - and the one that gets forgotten is
;; whichever of the three you were not editing, which is exactly the one
;; whose staleness you will not recognise when it bites.
;;
;; SO THIS IS ABOUT THE PROCESS AND NOT ABOUT A BUFFER, which is the whole of
;; why it is its own command and its own file.  Every other reload asks the
;; buffer what language it is - see the commentary in replique-repl.el for
;; the rule - and a command that reloads languages nothing on the screen is
;; written in has no buffer to ask.  It reloads what the PROCESS has open.
;;
;; NOTHING NEW REACHES THE PROCESS.  The Clojure and the ClojureScript are
;; the `#replique/reload' every reload sends, and the stylesheets are the
;; `:reload-css' op - see doc/protocol.md.  What is here is the order, which
;; repls, and one sentence for the three of them.
;;
;; THE ORDER IS CLOJURE, THEN EACH CLOJURESCRIPT RUNTIME, THEN THE
;; STYLESHEETS.  Clojure first because a ClojureScript compile expands
;; Clojure macros, so a ClojureScript reload run before them would compile
;; against the macros of the branch you just left.  The stylesheets last
;; because they are built by a program that neither reads Clojure nor is read
;; by it, so their place in the order is free and last is where their
;; sentence is easiest to put at the end of the other one.
;;
;; A LANGUAGE THAT WILL NOT COMPILE STOPS THE ONES AFTER IT.  A checkout that
;; does not compile is one thing wrong, and the second and third walls of
;; errors from it are the same thing wrong said twice more - the first one is
;; where you go and look.  The stylesheets are built all the same, since a
;; macro that will not compile has nothing to say about sass.
;;
;; A LANGUAGE THE PROCESS HAS NO REPL FOR IS NOT AN ERROR, which is the one
;; place this parts company with `replique-reload-all'.  That command is
;; asked for a language, by a buffer, and answers that there is no repl for
;; it because there is nothing else it could have been asked; this one is
;; asked for whatever is running, and a process running only Clojure is a
;; process with nothing wrong with it.  Nor is a project with no stylesheets.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-css)
(require 'replique-eval)
(require 'replique-process)
(require 'replique-repl)

;;; What the reload reads

(defconst replique-reload--stylesheet-suffixes
  '(".css" ".scss" ".sass" ".less")
  "The file names a stylesheet build might read.

By name and not by major mode, which is how `replique-reload-css\\=' decides
the same thing and for the same reason: the fact is the file, and which
mode you happen to read a .scss in is yours.")

(defun replique-reload--source-p (root)
  "Whether this buffer holds a file the reload would read, under ROOT.

What `save-some-buffers\\=' is given, so that what is offered to be saved is
what is about to be read - and only that.  BOTH LANGUAGES AND THE
STYLESHEETS, because all three are about to be built or compiled from the
disk, where `replique-reload-all\\=' offers the Clojure alone.

UNDER THE PROCESS\\='S DIRECTORY, which the other two do not ask about
because they are handed the one file they are about to read.  This is
handed nothing and would otherwise offer you every modified stylesheet
open in Emacs, including the ones belonging to a project this process has
never heard of."
  (and buffer-file-name
       ;; Both sides resolved, and compared as names: a project reached
       ;; through a symlink is one path to Emacs and another to the process,
       ;; and `file-in-directory-p' answers nil for a directory that is not
       ;; on the disk at all - which the directory of a process in a test is
       (string-prefix-p (file-name-as-directory (file-truename root))
                        (file-truename buffer-file-name))
       (or (derived-mode-p 'replique-clojure-mode)
           (seq-some (lambda (suffix)
                       (string-suffix-p suffix buffer-file-name t))
                     replique-reload--stylesheet-suffixes))))

;;; Which repls, and what they are called

(defun replique-reload--label (repl)
  "What to call REPL in a sentence about what was reloaded.

The language, and for ClojureScript the runtime too: a process with a
browser repl and a node repl open reloads both, and they are two separate
compiles of two separate programs.  Naming only the language would report
them as one thing done twice."
  (if (eq :cljs (replique-repl-dialect repl))
      (if-let* ((target (replique-repl-target repl)))
          (format "ClojureScript (%s)" (substring (symbol-name target) 1))
        "ClojureScript")
    "Clojure"))

(defun replique-reload--repls (process)
  "The repls of PROCESS to reload in, in the order to reload them.

ONE PER LANGUAGE AND RUNTIME, and not one per repl.  Two Clojure repls are
two threads reading from one set of namespaces, and two ClojureScript repls
on one target share the compile environment that decides what is stale - so
reloading both would be the second one finding that nothing has changed
since the first one loaded it, which is true and is not what was asked.

Clojure first - see the commentary.  Among the ClojureScript runtimes the
order is whatever the process lists, because there is nothing to say one
of them should go before another: they are separate programs."
  (let ((seen nil)
        (found nil))
    (dolist (repl (replique-process--repls process))
      (when (replique-repl-live-p repl)
        (let ((key (cons (replique-repl-dialect repl) (replique-repl-target repl))))
          (unless (member key seen)
            (push key seen)
            (push repl found)))))
    (setq found (nreverse found))
    (append (seq-filter (lambda (repl) (eq :clj (replique-repl-dialect repl))) found)
            (seq-remove (lambda (repl) (eq :clj (replique-repl-dialect repl))) found))))

(defun replique-reload--busy (repls)
  "The first of REPLS that is not ready to be sent a reload, or nil.

ASKED OF ALL OF THEM BEFORE ANY OF THEM IS SENT ANYTHING.
`replique-repl-send-code-sync\\=' refuses a repl that is busy, and refusing
the second one after the first has already recompiled the application
would leave the process half way between two branches with nothing said
about which half - the state this command exists to get out of."
  (seq-find (lambda (repl) (not (replique-repl--at-prompt repl))) repls))

;;; Doing it

(defun replique-reload--languages (repls)
  "Load what changed in each of REPLS in turn, and return what happened.

A plist.  :loaded holds the labels of the repls that loaded something,
:quiet those that had nothing to load, and :stopped the label of the one
that did not finish - after which the rest are not asked, for the reason
the commentary gives.

WHAT WAS LOADED IS NOT READ OUT OF THE ANSWER, only whether there was any.
The value of a reload is the list of files it loaded, printed, and it is
printed in the repl that loaded them, which is where a list of forty files
belongs.  What is told apart here is the empty list from a list, which is
an answer of exactly \"[]\" against anything else - what `pr-str' writes
for an empty vector, and not a guess about the shape of the rest."
  (let ((loaded nil)
        (quiet nil)
        (stopped nil))
    (dolist (repl repls)
      (unless stopped
        (let ((label (replique-reload--label repl))
              (frame (replique-reload--in repl t)))
          (cond
           ((not (equal "ret" (plist-get frame :tag)))
            (setq stopped label))
           ((equal "[]" (string-trim (or (plist-get frame :value) "")))
            (push label quiet))
           (t (push label loaded))))))
    (list :loaded (nreverse loaded) :quiet (nreverse quiet) :stopped stopped)))

(defun replique-reload--said (languages stylesheets)
  "The one sentence for what LANGUAGES and STYLESHEETS came to.

LANGUAGES is what `replique-reload--languages\\=' answered and STYLESHEETS
is the sentence the stylesheet reload came back with, or nil where this
project has none.

ONE SENTENCE FOR THE WHOLE COMMAND, and that is why this takes both at
once.  The three halves finish at three different moments, and three
messages in the echo area are the first two gone: the one anybody would
have wanted to read is whichever of them did not do what was expected,
and it is never the last one.

WHAT STOPPED IS NAMED AND NOT REPEATED.  The exception is in the repl that
threw it, whole and triaged, which is where it is read - saying it again
here would be the first line of it, in an echo area, with the rest cut."
  (let* ((loaded (plist-get languages :loaded))
         (quiet (plist-get languages :quiet))
         (stopped (plist-get languages :stopped))
         (parts (append
                 (when loaded
                   (list (format "loaded %s" (string-join loaded ", "))))
                 (when stopped
                   (list (format "the %s load stopped - see its repl" stopped)))
                 (when (and (null loaded) (null stopped))
                   (list (format "nothing to load in %s"
                                 (string-join quiet ", "))))
                 (when stylesheets (list stylesheets)))))
    (string-join parts " - ")))

;;;###autoload
(defun replique-reload-app ()
  "Bring the whole running application up to date with the disk.

Every language the process has a repl open on, and then this project\\='s
stylesheets: the Clojure, the ClojureScript of each runtime, and the .css
a build writes.  FOR A BRANCH THAT HAS JUST BEEN SWITCHED, where what
changed is not what you were editing and no buffer on the screen says
which of the three it was in - see the commentary.

Nobody names anything.  What changed in each language is the process\\='s to
know and is what \\[replique-reload-all] loads, one language at a time;
what the stylesheets are built from is this project\\='s to say and is what
\\[replique-reload-css] builds, one stylesheet at a time.  This is those,
in the order they have to happen in, with one sentence at the end.

CLOJURE, THEN EACH CLOJURESCRIPT RUNTIME, THEN THE STYLESHEETS.  A
ClojureScript compile expands Clojure macros, so the Clojure has to be
loaded first or the compile is against the branch you left.  A language
that will not compile stops the languages after it and is named; its
exception is in its own repl, which is where it is read.  The stylesheets
are built anyway - sass has no opinion about a macro.

A LANGUAGE THE PROCESS HAS NO REPL FOR IS SKIPPED, and so is a project
that says nothing about stylesheets.  Which is the difference between this
and the commands it is made of: they are asked for one thing and say so
when it cannot be done, and this is asked for whatever is running.

WHAT IS BUILT AND COMPILED IS THE DISK, so the modified buffers of this
project are offered to be saved first - the Clojure and the stylesheets
both, and nothing outside the directory the process was started in.

Every repl is asked whether it is ready before any of them is sent
anything, so that a repl busy with something else is said so while the
application is still on one branch rather than half way onto another."
  (interactive)
  (let* ((process (replique-process-ensure))
         ;; Where the build runs and what its paths are relative to - the
         ;; process's directory, as `replique-reload-css' uses it
         (root (or (replique-process--directory process) default-directory))
         (repls (replique-reload--repls process)))
    (unless repls
      (user-error "This process has no repl open - M-x replique-repl"))
    (when-let* ((busy (replique-reload--busy repls)))
      (user-error "The %s repl is busy with something else"
                  (replique-reload--label busy)))
    (save-some-buffers nil (lambda () (replique-reload--source-p root)))
    (let ((languages (replique-reload--languages repls)))
      (if (not (replique-css-configured-p))
          (message "replique: %s" (replique-reload--said languages nil))
        (let ((failed (replique-css-build root)))
          (if failed
              ;; As the build printed it, under the sentence rather than
              ;; instead of it: what is wrong with a stylesheet is something
              ;; sass has already said better than this could, and what the
              ;; languages did is still the answer to half of what was asked
              (message "replique: %s - the stylesheet build failed\n%s"
                       (replique-reload--said languages nil) failed)
            (replique-css--reload
             (replique-css-outputs-in root) process
             (lambda (files frames)
               (message "replique: %s"
                        (replique-reload--said
                         languages
                         (replique-css--sentence files frames)))))))))))

(provide 'replique-reload)

;;; replique-reload.el ends here
