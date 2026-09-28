;;; replique-stale.el --- What has to be loaded again  -*- lexical-binding: t; -*-

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

;; What a reload would load, looked at before it does it.
;;
;; Two lists, because they are two different facts about a file.  The copy
;; on the disk is newer than what the process read, which is a file that
;; was edited; or the file was not edited and what the process holds of it
;; is out of date all the same, because it expands a macro of a file that
;; was.  The second is the half nobody can work out by looking at their
;; buffers, and it is the reason this is worth a command of its own: what
;; needs compiling is not the same list as what was typed in.
;;
;; Asked of the process rather than worked out here.  What changed is a
;; comparison against the time the process read each file, which nothing
;; but the process knows, and what that made stale is a graph its compiler
;; recorded while it compiled - see `replique-reload-all', which is the
;; same question answered by doing it.
;;
;; AND ASKED ABOUT ONE LANGUAGE, OR ABOUT ALL OF THEM.  A process can hold a
;; Clojure application and a ClojureScript one at once, each with its own files
;; behind the disk, and what is stale in one says nothing about the other.
;; `replique-stale' is asked from a buffer and carries that buffer's dialect,
;; the way every other question about a name does; `replique-stale-app' is
;; asked about the process and carries one question per repl, which is the
;; question `replique-reload-app' is about to answer by doing it - and the
;; moment worth looking before leaping is exactly that one, after a branch has
;; been switched.
;;
;; THE BUFFER REMEMBERS WHICH IT WAS: what `g' asks again has to be the
;; question this buffer answered, and the buffer is not a Clojure buffer of
;; either kind, so asking it afresh from here would ask about whatever repl the
;; commands are pointed at now.  `l' reloads the same thing for the same
;; reason - that language, or the whole application.
;;
;; AND THE STYLESHEETS, WHICH ARE A WEAKER FACT AND SAY SO.  The other two
;; lists are what the process compiled and when; this one is two modification
;; times compared in Emacs, because the process has never heard of a .scss and
;; what reads which partial is sass's to know.  It is still the question
;; somebody has - is the .css on disk behind the .scss - and it is the third of
;; `replique-reload-app' that nothing said anything about until now.
;;
;; Nothing waits for the answer.  The process reads the modification time
;; of every file it has compiled, which is a question about a disk rather
;; than about memory, so the buffer is written when the answer arrives - and
;; where there is a question per repl, when the last of them arrives.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-css)
(require 'replique-eval)
(require 'replique-name)
(require 'replique-process)
(require 'replique-reload)
(require 'replique-repl)
(require 'replique-symbol)

(defvar-local replique-stale--sections nil
  "What the process answered, as the buffer showing it was written.

A list of plists, one per question asked, in the order they are reloaded
in:

  :label         what to call the language, `replique-reload--label\='
  :dialect-keys  the keys that question carried, nil for Clojure
  :found         the frame that came back

The stylesheets are not in here.  They are not something the process was
asked - see `replique-stale--stylesheets\=' - and putting them in a list
whose every other entry is an answer would be the one place in this
buffer where the shape lies about where a fact came from.")

(defvar-local replique-stale--process nil
  "The process the buffer was written from, and is written again from.")

(defvar-local replique-stale--scope nil
  "What this buffer is about: `here\=' for one language, `app\=' for all of them.

Kept because this buffer is not the buffer the question was asked from.
Asking afresh would ask about whatever repl the commands are pointed at
now, and `g\=' has to ask the question this buffer answered; `l\=' reloads
what `g\=' would ask about, which is the same rule read the other way.

`app\=' is also what says the stylesheets belong here: they are a third of
`replique-reload-app\=' and no part of a question asked about one
language.")

(defvar-local replique-stale--stylesheets nil
  "The built stylesheets a source has got ahead of, or nil.

As `replique-css-stale-in\=' answers them.

Worked out here rather than asked, because the process has never heard of
a .scss.  Which is also why it is kept apart from the answers: see
`replique-stale--sections\='.")

;;; Rendering

(defun replique-stale--label (found directory)
  "Return what to call the file FOUND, for somebody working in DIRECTORY.

The path it has under the directory the process was started in, which is
how a developer names their own files - and the whole path where the file
is somewhere else, since a relative name climbing out of the project says
less than the path it is a longer way of writing.

A file inside an archive has no path: it is named as the entry it is,
beside the archive holding it, which is how this protocol writes one
everywhere."
  (let ((file (plist-get found :file))
        (entry (plist-get found :entry)))
    (cond
     (entry (format "%s in %s" entry (file-name-nondirectory file)))
     ((and directory (string-prefix-p (expand-file-name directory) file))
      (file-relative-name file directory))
     (t (abbreviate-file-name file)))))

(defun replique-stale--open (found)
  "Open the file FOUND names."
  (pop-to-buffer (or (replique-symbol-visit found)
                     (user-error "Replique: %s is not there to be opened"
                                 (plist-get found :file)))))

(defun replique-stale--insert (files directory)
  "Insert FILES, each as something that opens it, named from DIRECTORY."
  (dolist (found files)
    (insert "  "
            (propertize (buttonize (replique-stale--label found directory)
                                   #'replique-stale--open found)
                        'help-echo "RET or mouse-1: open this file")
            "\n")))

(defun replique-stale--open-path (path)
  "Open the file at PATH."
  (if (file-exists-p path)
      (pop-to-buffer (find-file-noselect path))
    (user-error "Replique: %s is not there to be opened" path)))

(defun replique-stale--path-label (path directory)
  "Return what to call PATH, for somebody working in DIRECTORY.

`replique-stale--label\=' for a file nobody was asked about: a stylesheet
is a path this worked out rather than a place the process answered with,
so there is no entry to name and no archive it could be inside."
  (if (and directory (string-prefix-p (expand-file-name directory) path))
      (file-relative-name path directory)
    (abbreviate-file-name path)))

(defun replique-stale--insert-paths (pairs directory)
  "Insert PAIRS, each (OUTPUT . SOURCE), named from DIRECTORY.

The OUTPUT is what opens, because the output is the file that is behind:
what a page fetches, and what a build would write over.  The source is
named beside it and does not open - it is there to say what the output is
behind, and it is one of many."
  (dolist (pair pairs)
    (insert "  "
            (propertize (buttonize (replique-stale--path-label (car pair) directory)
                                   #'replique-stale--open-path (car pair))
                        'help-echo "RET or mouse-1: open this file")
            (format "  (%s is newer)"
                    (replique-stale--path-label (cdr pair) directory))
            "\n")))

(defun replique-stale--render-section (section directory named)
  "Write SECTION into the current buffer, naming files from DIRECTORY.

NAMED says whether to write the language above it, which is asked rather
than worked out: one Clojure answer reads as it always did, and every
other arrangement - ClojureScript, or more than one of them - has to say
which is which."
  (let* ((found (plist-get section :found))
         (cljs (and (plist-get section :dialect-keys) t))
         (changed (plist-get found :changed))
         (stale (plist-get found :stale))
         (message (plist-get found :message)))
    (when named (insert (plist-get section :label) "\n\n"))
    (cond
     ;; A process that keeps no track of what it compiled refuses the
     ;; question, and the refusal is a sentence of its own saying what to
     ;; start the process on instead.  Shown as it was written: a heading with
     ;; two empty lists under it would report a process that cannot answer as
     ;; a process with nothing to load.
     ((equal "error" (plist-get found :tag))
      (insert "  " (or message "the process would not answer") "\n"))
     ((and (null changed) (null stale))
      (insert (if cljs
                  "  Nothing has changed since this process compiled it.\n"
                "  Nothing has changed since this process read it.\n")))
     (t
      (insert (if cljs
                  "Changed since the process compiled them\n\n"
                "Changed since the process read them\n\n"))
      (replique-stale--insert changed directory)
      (when stale
        ;; "a file that has changed" rather than "a file above", which is
        ;; true of Clojure and not of ClojureScript: a .cljs file expands
        ;; macros written in .clj files, and those are not in the list above
        ;; - they are Clojure files, and this answer is about ClojureScript
        ;; ones.
        (insert "\nNot changed, and out of date all the same: these expand a macro\n"
                "of a file that has changed, and hold the expansion the old one made\n\n")
        (replique-stale--insert stale directory))))
    ;; Under the files rather than above them, and only where there are files:
    ;; it is the answer to "and then what", and a language with nothing to load
    ;; has no then.
    (when (and cljs (or changed stale)
               (not (plist-get found :connected)))
      (insert "\n  Nothing is connected to this runtime, so a reload would compile\n"
              "  all of it and land nowhere.  Open the application, or check that\n"
              "  its main module names this process - M-x replique-main-js.\n"))))

(defun replique-stale--render ()
  "Write what the process answered into the current buffer."
  (let* ((inhibit-read-only t)
         (process replique-stale--process)
         (directory (and process (replique-process--directory process)))
         (sections replique-stale--sections)
         ;; Named where naming them tells them apart, which is every
         ;; arrangement except the one this buffer has always had
         (named (or (cdr sections)
                    (plist-get (car sections) :dialect-keys)
                    replique-stale--stylesheets)))
    (erase-buffer)
    (dolist (section sections)
      (unless (eq section (car sections)) (insert "\n"))
      (replique-stale--render-section section directory named))
    (when replique-stale--stylesheets
      (insert "\nStylesheets\n\n"
              "Built, and a file the build would read is newer.  Which is two\n"
              "modification times compared here rather than anything the process\n"
              "compiled: what reads which partial is sass's to know, and a build\n"
              "rebuilds the entry point whatever this says.\n\n")
      (replique-stale--insert-paths replique-stale--stylesheets directory))
    (goto-char (point-min))))

;;; The mode

(defvar replique-stale-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'replique-stale-refresh)
    (define-key map (kbd "l") #'replique-stale-reload)
    map)
  "Keymap of a staleness buffer.")

(define-derived-mode replique-stale-mode special-mode "Replique-Stale"
  "Major mode for looking at what has to be loaded again.

\\{replique-stale-mode-map}"
  (setq-local truncate-lines nil))

;;; Asking

(defun replique-stale--show (process scope sections stylesheets)
  "Show what PROCESS said in SECTIONS, with STYLESHEETS under them.

Returns the buffer.  SCOPE is what this buffer is about, kept for
`replique-stale-refresh' and `replique-stale-reload' - see
`replique-stale--scope'."
  (let ((buffer (get-buffer-create "*replique-stale*")))
    (with-current-buffer buffer
      ;; Before anything buffer local is set: the mode kills them
      (replique-stale-mode)
      (setq replique-stale--process process
            replique-stale--scope scope
            replique-stale--sections sections
            replique-stale--stylesheets stylesheets)
      (replique-stale--render))
    (pop-to-buffer buffer)
    buffer))

(defun replique-stale--ask (process dialect-keys)
  "Ask PROCESS what one language has to load again, and show the answer.

DIALECT-KEYS says which, nil being Clojure.  The refusal a process that
kept no track of what it compiled answers with is shown in the echo area
rather than in a buffer: one question was asked, it was not answered, and
a window holding that sentence and nothing else is a window for nothing."
  (replique-process-request
   process (append (list :op :stale) dialect-keys)
   (lambda (frame)
     (if (equal "error" (plist-get frame :tag))
         (message "replique: %s" (plist-get frame :message))
       (replique-stale--show
        process 'here
        (list (list :label (if dialect-keys "ClojureScript" "Clojure")
                    :dialect-keys dialect-keys
                    :found frame))
        nil)))))

(defun replique-stale--ask-app (process)
  "Ask PROCESS what every repl it has open would load, and show the answers.

ONE QUESTION PER REPL, which is one per language and runtime rather than
one per repl - `replique-reload--repls' decides that, and the order it
gives is the order they are reloaded in.  They go out together and come
back in whatever order the process answers them, so the buffer is written
when the last one has arrived.

A REFUSAL IS SHOWN RATHER THAN THROWN AWAY HERE, which is where this
parts company with `replique-stale--ask'.  One question refused is
nothing to show; one of four refused, beside three that were answered, is
a fact about this process worth reading next to the rest.

And the stylesheets under them, which are worked out here and asked of
nobody - see `replique-css-stale-in'."
  (let ((repls (replique-reload--repls process)))
    (unless repls
      (user-error "This process has no repl open - M-x replique-repl"))
    (let ((answers (make-hash-table :test #'eq))
          (left (length repls))
          (root (or (replique-process--directory process) default-directory)))
      (dolist (repl repls)
        (replique-process-request
         process (append (list :op :stale) (replique-reload--dialect-keys repl))
         (lambda (frame)
           (puthash repl frame answers)
           (setq left (1- left))
           (when (zerop left)
             (replique-stale--show
              process 'app
              (mapcar (lambda (repl)
                        (list :label (replique-reload--label repl)
                              :dialect-keys (replique-reload--dialect-keys repl)
                              :found (gethash repl answers)))
                      repls)
              (replique-css-stale-in root)))))))))

(defun replique-stale-refresh ()
  "Ask again, of the process this buffer was written from.

That process rather than whichever one the commands act on now: a buffer
that answered about one process and then answered about another, under
the same heading, would be two answers nothing tells apart.

And the same question.  A buffer showing one language goes on showing
that language, and one showing the whole process goes on showing the
whole process - which for the second means the repls it has NOW, since
that is what the question is about."
  (interactive)
  (let ((process (or replique-stale--process
                     (user-error "This buffer was written from no process"))))
    (if (eq 'app replique-stale--scope)
        (replique-stale--ask-app process)
      (replique-stale--ask process
                           (plist-get (car replique-stale--sections)
                                      :dialect-keys)))))

(defun replique-stale-reload ()
  "Load what this buffer is showing.

Told what to reload rather than left to work it out: this buffer is not a
buffer of either language, so `replique-reload-all' would otherwise
reload whatever repl the commands are pointed at now - which could be the
one this answer is not about.

A buffer showing the whole process reloads the whole process,
stylesheets and all, which is what it is showing."
  (interactive)
  (if (eq 'app replique-stale--scope)
      (replique-reload-app)
    (replique-reload-all
     nil (if (plist-get (car replique-stale--sections) :dialect-keys) :cljs :clj))))

;;;###autoload
(defun replique-stale ()
  "Show what loading everything that changed would load.

The same question `replique-reload-all' answers by doing it, asked
without doing it: nothing is compiled, and nothing in the process
changes.

Two lists.  The files whose copy on the disk is newer than what the
process read - what was edited - and, apart from those, the files that
were not edited and are out of date all the same, because they expand a
macro of a file that was and hold the expansion the old one made.  The
second list is the one worth looking at: what needs compiling is not the
same as what was typed in, and nothing in a buffer says which files
those are.

About the language of this buffer, which a process holding a Clojure
application and a ClojureScript one at once has two answers for: a .cljs
buffer asks about the ClojureScript, a .clj buffer about the Clojure, and a
.cljc buffer about whichever repl the commands are pointed at - the three
cases every question about a name follows.  Every language the process
has open at once, and the stylesheets with them, is
\\[replique-stale-app].

Each one opens.  \\<replique-stale-mode-map>\\[replique-stale-refresh] \
asks again, \\[replique-stale-reload] loads them.

Only a process whose compiler wrote down what it compiled can answer
this.  One that cannot says so, and says what to start it on instead."
  (interactive)
  (replique-stale--ask (or (replique-name-process)
                           (user-error "No replique process"))
                       (replique-dialect-keys)))

;;;###autoload
(defun replique-stale-app ()
  "Show what bringing the whole application up to date would load.

What \\[replique-reload-app] is about to do, asked without doing it:
every language the process has a repl open on, and this project's
stylesheets, with nothing compiled and nothing in the process changed.

FOR THE MOMENT BEFORE A BRANCH THAT HAS JUST BEEN SWITCHED IS RELOADED,
which is the moment worth looking before leaping: what changed is not
what you were editing, no buffer on the screen says which language it was
in, and the list of files nobody can predict is exactly what this is.

A section per language and runtime, in the order they would be reloaded -
the Clojure, then each ClojureScript runtime - each with the two lists
\\[replique-stale] shows.  A ClojureScript runtime with something to load
and nothing connected to it says so under its files: that reload would
compile the whole program correctly and land nowhere.

AND THE STYLESHEETS, WHICH ARE A WEAKER FACT AND SAY SO.  The other
sections are what the process compiled and when.  This one is the built
.css compared against the files under `replique-css-entry', in Emacs,
because the process has never heard of a .scss - so it does not know
which partial the entry point reads, and a build rebuilds the entry
whatever it says.  It still answers the question somebody has: is the
.css a page fetches behind the .scss.  Absent where this project says
nothing about stylesheets, and absent where it says only how to run its
build - a command is not a list of files.

Each one opens.  \\<replique-stale-mode-map>\\[replique-stale-refresh] \
asks again, \\[replique-stale-reload] reloads the application.

A LANGUAGE WHOSE QUESTION IS REFUSED IS SHOWN AS REFUSED rather than left
out: only a process whose compiler wrote down what it compiled can answer
this, and one of four sections saying so beside three that answered is
worth reading."
  (interactive)
  (replique-stale--ask-app (replique-process-ensure)))

(provide 'replique-stale)

;;; replique-stale.el ends here
