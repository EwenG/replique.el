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
;; the `#replique/reload' every reload sends, the plan is the `:stale' op
;; `replique-stale-app' asks, and the stylesheets are the `:reload-css' op -
;; see
;; doc/protocol.md.  What is here is the order, which repls, and one sentence
;; for the three of them.
;;
;; AND IT DOES NOT HOLD THE EDITOR.  Three compiles in a row is a long time to
;; be gone, and it is not what the order needs: what the ClojureScript needs is
;; that the Clojure ENDED, and a callback knows that as well as a blocked Emacs
;; does.  So each step is sent from the end of the one before it, through
;; `replique-repl-send-code-then' - which is the same machinery
;; `replique-repl-send-code-sync' spins on, with the spinning taken out.  What
;; is paid for that is that the repls are free in between, and a form typed
;; into one of them is a form the next step has to notice and stand down for:
;; see `replique-reload--step'.
;;
;; WHAT EACH LANGUAGE WOULD LOAD IS ASKED BEFORE ANY OF IT IS COMPILED.  One
;; round trip, no compiling, and it is what lets the sentence at the end say
;; anything true about a language that did nothing: one with nothing to load is
;; not asked to load it, and one with something to load and no runtime
;; connected is named rather than compiled for - that reload would build the
;; whole program correctly and land nowhere.
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
(require 'replique-classpath)
(require 'replique-css)
(require 'replique-eval)
(require 'replique-main-js)
(require 'replique-process)
(require 'replique-repl)

;;; What the reload reads

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
                     replique-css-source-suffixes))))

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

(defun replique-reload--dialect-keys (repl)
  "The dialect keys a question about REPL\='s program carries.

`replique-dialect-keys\=' asked of a repl rather than of the current
buffer, which is what a command about the whole process needs: it is not
in a buffer of either language, and there is more than one answer.  Nil
for Clojure, which is what an absent `:dialect\=' means to the process as
well."
  (when (eq :cljs (replique-repl-dialect repl))
    (append (list :dialect :cljs)
            (when-let* ((target (replique-repl-target repl)))
              (list :target target)))))

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

;;; What each repl would load, asked before anything is compiled

(defun replique-reload--plan-of (repl frame)
  "What to do about REPL, given the FRAME it answered the `:stale' op with.

One of five, and the useful one is the last:

  :ask      the question could not be answered - send the reload anyway
  :unread   this process has read no files of this language at all
  :nothing  nothing has changed and nothing went stale
  :blocked  there is something to load and nowhere to run it
  :load     there is something to load

:ask IS WHAT A PROCESS THAT KEEPS NO TRACK ANSWERS, and it is why this
asks first and does not decide on the answer alone.  Only a process whose
compiler wrote down what it compiled can say what is stale; one that
cannot refuses the op - and refusing to reload on the strength of that
would turn a process that has always reloaded into one that does not.  So
the reload goes out, and the repl gives the refusal it has always given,
in the repl, where it says what to start the process on instead.

:blocked IS CLOJURESCRIPT AND ONLY CLOJURESCRIPT.  A Clojure reload ends
when the files have been loaded in the process; a ClojureScript one has a
second act - the recompiled bodies have to be run in the page or the node
process - and a browser runtime with no page connected is a reload that
would compile everything correctly and land nowhere.  Which the answer
says, because it is half of what would happen if a reload were asked for.

:unread IS THE OTHER WAY OF ANSWERING NOTHING, and it is a different fact.
A Clojure process holds what its compiler read UNDER THE SINK, which is
what \[replique-load-file] and a reload push - so a namespace that arrived
by `require\=', at a prompt or from an init script, is loaded, is running,
and is not in the model.  Such a process answers with two empty lists and
will go on answering that whatever is edited, and calling that \"nothing to
load\" would be telling somebody their application is up to date on the
strength of a process that has never heard of it.  The `analysed\=' count
is what tells the two apart; an answer with no such count in it is an older
process than this and is read the way it always was.

:nothing IS CHECKED FIRST, including where nothing is connected: a repl
with nothing to load has nothing to complain about, and reporting the
missing page of a reload that was not going to do anything would be
reporting a problem nobody has."
  (cond
   ((equal "error" (plist-get frame :tag)) :ask)
   ((eql 0 (plist-get frame :analysed)) :unread)
   ((and (null (plist-get frame :changed))
         (null (plist-get frame :stale)))
    :nothing)
   ((and (eq :cljs (replique-repl-dialect repl))
         (not (plist-get frame :connected)))
    :blocked)
   (t :load)))

(defun replique-reload--plan (process repls done)
  "Ask PROCESS what each of REPLS would load, and call DONE with the answer.

DONE is called with a list of (REPL . PLAN) in the order REPLS were given,
PLAN being what `replique-reload--plan-of' decided.

ASKED BEFORE ANYTHING IS COMPILED, and it costs a round trip and no
compiling: the `:stale' op reads the file times and walks the graph and
builds nothing.  What it buys is everything the sentence at the end can
say that it could not say before - a language with nothing to load is not
asked to load it, and a page that is not there is named instead of being
compiled for.

ASYNCHRONOUS, and that is why DONE is a function: the ops go out together
and come back in whatever order the process answers them, so there is no
moment at which this could hand anything back.  Counted down rather than
collected in order, the way `replique-css--reload' counts its answers, and
put back in the order asked because that is the order they are reloaded
in."
  (let ((answers (make-hash-table :test #'eq))
        (left (length repls)))
    (dolist (repl repls)
      (replique-process-request
       process (append (list :op :stale) (replique-reload--dialect-keys repl))
       (lambda (frame)
         (puthash repl (replique-reload--plan-of repl frame) answers)
         (setq left (1- left))
         (when (zerop left)
           (funcall done
                    (mapcar (lambda (repl) (cons repl (gethash repl answers)))
                            repls))))))))

;;; The classpath, before anything is asked of it

(defun replique-reload--classpath (process then)
  "Bring the classpath of PROCESS up to date, then call THEN with a sentence.

THE FIRST STEP, AND IT IS BEFORE THE PLAN.  A branch switched can be a
classpath changed - a library added, a worktree pointed at under a
process reading it through links - and every step after this one reads
the classpath: what is stale is answered out of the files on it, and a
reload compiles against it.  So a reading that is due is done, and what
the deps files would add is asked about, before anything else is asked.
See replique-classpath.el.

THEN is not called where the process is being restarted instead: the
repls this would reload are on their way out, and the process that comes
back loads the application fresh."
  (replique-classpath-check
   process
   (lambda (report)
     (replique-classpath-act
      process report
      (lambda (sentence go-on)
        (if go-on
            (funcall then sentence)
          (message "replique: %s" sentence)))))
   nil t))

;;; Doing it

(defun replique-reload--soon (function)
  "Call FUNCTION from the command loop rather than from where we are.

EVERY STEP OF THE CHAIN GOES THROUGH THIS, because every step is reached
from a process filter: a frame arrived, and what runs on its heels runs
while Emacs is in the middle of reading from a socket.  Sending the next
form from there would be safe enough - it is a write - but building
stylesheets is a subprocess and writing the sentence is the echo area,
and neither belongs inside a read.  A zero timer is the ordinary way to
say `not from here'."
  (run-at-time 0 nil function))

(defun replique-reload--step (steps state done)
  "Reload in the first of STEPS, then in the rest, then call DONE with STATE.

STEPS is what `replique-reload--plan' answered, filtered to the repls
there is anything to send to.  STATE is the plist
`replique-reload--said' reads, built up as the chain goes.

ONE AT A TIME AND IN ORDER, which is the whole reason this is a chain
rather than a fan-out: a ClojureScript compile expands Clojure macros, so
the Clojure has to have been loaded before the ClojureScript starts or
the compile is against the branch that was left.  An order is not a wait,
though - what the next step needs is that the last one ENDED, and a
callback knows that as well as a blocked editor does.

A LANGUAGE THAT WILL NOT COMPILE STOPS THE ONES AFTER IT, for the reason
the commentary gives: the second and third walls of errors from a
checkout that does not compile are the same thing wrong said twice more.

AND SO DOES A REPL SOMEBODY HAS STARTED USING, OR CLOSED.  Nothing is held
between two steps, which is what makes this not block the editor and is
also what leaves the repls free to be used - and a repl is asked again
before it is sent anything.  The frame that ends an evaluation carries
nothing saying which evaluation it ended, so a reload sent behind
somebody\\='s form would be a reload whose answer is their form\\='s, and
there is no reading of that worth attempting.  A repl that is gone is the
same thing with nothing to wait for at all.  Either is said, and the chain
stops."
  (if (null steps)
      (funcall done state)
    (let* ((repl (car (car steps)))
           (label (replique-reload--label repl)))
      (if-let* ((why (cond ((not (replique-repl-live-p repl)) :gone)
                           ((not (replique-repl--at-prompt repl)) :busy))))
          (funcall done (plist-put state why label))
        (replique-repl-send-code-then
         repl (replique-reload-directive) nil
         (lambda (frame)
           (replique-reload--soon
            (lambda ()
              (cond
               ((not (equal "ret" (plist-get frame :tag)))
                (funcall done (plist-put state :stopped label)))
               (t
                (let ((key (if (equal "[]" (string-trim
                                            (or (plist-get frame :value) "")))
                               :quiet
                             :loaded)))
                  (replique-reload--step
                   (cdr steps)
                   (plist-put state key
                              (append (plist-get state key) (list label)))
                   done))))))))))))

(defun replique-reload--said (languages stylesheets)
  "The one sentence for what LANGUAGES and STYLESHEETS came to.

LANGUAGES is what the chain of `replique-reload--step' built and
STYLESHEETS is the sentence the stylesheet reload came back with, or nil
where this project has none.

ONE SENTENCE FOR THE WHOLE COMMAND, and that is why this takes both at
once.  The halves finish at different moments, and three messages in the
echo area are the first two gone: the one anybody would have wanted to
read is whichever of them did not do what was expected, and it is never
the last one.

WHAT STOPPED IS NAMED AND NOT REPEATED.  The exception is in the repl that
threw it, whole and triaged, which is where it is read - saying it again
here would be the first line of it, in an echo area, with the rest cut."
  (let* ((classpath (plist-get languages :classpath))
         (loaded (plist-get languages :loaded))
         (quiet (plist-get languages :quiet))
         (unread (plist-get languages :unread))
         (blocked (plist-get languages :blocked))
         (stopped (plist-get languages :stopped))
         (busy (plist-get languages :busy))
         (gone (plist-get languages :gone))
         (parts (append
                 ;; First, because it happened first, and because what is
                 ;; loaded after it was loaded against it
                 (when classpath (list classpath))
                 (when loaded
                   (list (format "loaded %s" (string-join loaded ", "))))
                 (when blocked
                   (list (format "%s has something to load and nowhere to run it - no runtime is connected"
                                 (string-join blocked ", "))))
                 (when stopped
                   (list (format "the %s load stopped - see its repl" stopped)))
                 (when busy
                   (list (format "the %s repl was being used, so it was left alone"
                                 busy)))
                 (when gone
                   (list (format "the %s repl closed while this was running" gone)))
                 ;; Said whatever else happened, because it is not a report of
                 ;; what this command did - it is the reason a language is
                 ;; missing from everything above, and the one thing that would
                 ;; otherwise read as an application that is up to date
                 (when unread
                   (list (format (concat "nothing has been loaded into %s through"
                                         " this process, so it has nothing to"
                                         " bring up to date")
                                 (string-join unread ", "))))
                 (when (and (null loaded) (null stopped) (null busy) (null blocked)
                            (null gone) (null unread))
                   (list (format "nothing to load in %s"
                                 (string-join quiet ", "))))
                 (when stylesheets (list stylesheets)))))
    (string-join parts " - ")))

(defun replique-reload--stylesheets (process root languages)
  "Build and reload the stylesheets of PROCESS under ROOT, and say what happened.

The tail of the command: LANGUAGES is what the chain built, and this is
what turns it into the one sentence.  Called once the languages have
finished, whether they finished well or not - sass has no opinion about a
macro, and a checkout whose Clojure does not compile still has
stylesheets that do."
  (if (not (replique-css-configured-p))
      (message "replique: %s" (replique-reload--said languages nil))
    (let ((failed (replique-css-build root)))
      (if failed
          ;; As the build printed it, under the sentence rather than instead
          ;; of it: what is wrong with a stylesheet is something sass has
          ;; already said better than this could, and what the languages did
          ;; is still the answer to half of what was asked
          (message "replique: %s - the stylesheet build failed\n%s"
                   (replique-reload--said languages nil) failed)
        (replique-css--reload
         (replique-css-outputs-in root) process
         (lambda (files frames)
           (message "replique: %s"
                    (replique-reload--said
                     languages
                     (replique-css--sentence files frames)))))))))

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

IT DOES NOT HOLD THE EDITOR WHILE IT RUNS.  Each language is sent when the
one before it has ended, which is an order and not a wait: the compiling
happens in the process and Emacs goes on being Emacs.  What is being
loaded is said in each repl as it happens, a line per file, named before
the file is compiled - so a reload that is taking a while says where it
is, and one that never comes back says what it is inside.

CLOJURE, THEN EACH CLOJURESCRIPT RUNTIME, THEN THE STYLESHEETS.  A
ClojureScript compile expands Clojure macros, so the Clojure has to be
loaded first or the compile is against the branch you left.  A language
that will not compile stops the languages after it and is named; its
exception is in its own repl, which is where it is read.  The stylesheets
are built anyway - sass has no opinion about a macro.

WHAT EACH LANGUAGE WOULD LOAD IS ASKED FIRST, which costs a round trip and
no compiling - see \\[replique-stale-app], which is this question on its
own.  A language with nothing to load is not asked to load it, and a
ClojureScript runtime with something to load and no page connected is
named rather than compiled for: that reload would build everything
correctly and land nowhere, and the page that is not there is the thing
to be told about.

A LANGUAGE THE PROCESS HAS NO REPL FOR IS SKIPPED, and so is a project
that says nothing about stylesheets.  Which is the difference between this
and the commands it is made of: they are asked for one thing and say so
when it cannot be done, and this is asked for whatever is running.

WHAT IS BUILT AND COMPILED IS THE DISK, so the modified buffers of this
project are offered to be saved first - the Clojure and the stylesheets
both, and nothing outside the directory the process was started in.

Every repl is asked whether it is ready before any of them is sent
anything, and again before each of them is sent anything, so that a repl
somebody started using while this was running is left alone rather than
sent a reload whose answer would be their form\\='s.

AND THE CLASSPATH BEFORE ANY OF IT.  A checkout can change what is on it
as well as what is in it, and everything after reads it - so a reading the
process owes is done, what the deps files would add is offered, and what
it cannot follow without a restart offers one.  See
\\[replique-sync-classpath], which is that on its own."
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
    (replique-reload--classpath
     process
     (lambda (classpath)
       (replique-reload--plan
        process repls
        (lambda (plans)
          (replique-reload--soon
           (lambda ()
             (let ((state (when classpath (list :classpath classpath))))
               (dolist (plan plans)
                 (let ((key (pcase (cdr plan)
                              (:nothing :quiet)
                              (:unread :unread)
                              (:blocked :blocked))))
                   (when key
                     (setq state
                           (plist-put state key
                                      (append (plist-get state key)
                                              (list (replique-reload--label
                                                     (car plan)))))))))
               (replique-reload--step
                (seq-filter (lambda (plan) (memq (cdr plan) '(:load :ask))) plans)
                state
                (lambda (languages)
                  (replique-reload--stylesheets process root languages))))))))))))

;;; After the links under a process moved

(defun replique-reload--rebaseline (process from unchanged)
  "Tell PROCESS that UNCHANGED, under its directory, hold what they did under FROM.
UNCHANGED are paths relative to the process\='s directory.  The process
takes those as loaded where what it loaded is what FROM holds, so a reload
after this loads what changed rather than everything whose modification
time did.  A process too old to know the op answers with an error, which
is said and changes nothing."
  (let* ((root (replique-process--directory process))
         (frame (replique-process-request-sync
                 process
                 (list :op :rebaseline
                       :root (directory-file-name (expand-file-name root))
                       :from (directory-file-name (expand-file-name from))
                       :unchanged (vconcat unchanged)))))
    (if (equal "error" (plist-get frame :tag))
        (message "replique: could not tell %s what is unchanged: %s"
                 (replique-process--id process) (plist-get frame :message))
      (message "replique: %d Clojure and %d ClojureScript files are the same in %s"
               (or (plist-get frame :clojure) 0)
               (or (plist-get frame :clojurescript) 0)
               (file-name-nondirectory (directory-file-name from))))))

;;;###autoload
(defun replique-relinked (process to &optional from unchanged)
  "Bring PROCESS up to date after the links under its directory moved to TO.

For a process whose directory is a tree of links into a checkout, once
those links point into another one: TO is the checkout they point into
now, FROM the one they pointed into before, and UNCHANGED the files,
relative to either, whose content the two have in common.  Whatever moved
the links knows all three and the process knows none of them.

In order: PROCESS becomes the current one, the modified buffers under TO
are offered to be saved - `replique-reload-app' offers the ones under the
process\='s own directory, and the buffers are open on the checkout - the
main modules are moved to the process\='s port, since each checkout has its
own naming whichever port last wrote it, the process is told what did not
change where FROM and UNCHANGED are given, and the application is
reloaded, which reads the classpath again first."
  (setq replique-current-process process)
  (save-some-buffers nil (lambda () (replique-reload--source-p to)))
  (replique-refresh-main-js process)
  (when (and from unchanged)
    (replique-reload--rebaseline process from unchanged))
  (replique-reload-app))

(provide 'replique-reload)

;;; replique-reload.el ends here
