;;; replique-process.el --- Starting and finding replique processes  -*- lexical-binding: t; -*-

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

;; A replique process is either one Emacs started, or one that was already
;; running and left its description in .replique/processes.
;;
;; Both report what they print the same way: as events on the control
;; connection.  A process Emacs started also writes to a pipe Emacs reads,
;; and that pipe is what carries the startup line, along with everything the
;; jvm and the clojure script say before there is a process to connect to.
;; It stops being shown once the control connection is up - from there the
;; two would say the same thing - and it goes on being read, since a pipe
;; nobody reads fills up and stops the process writing to it.
;;
;; The process outlives Emacs on purpose, which is why it is started under
;; nohup: Emacs hangs up on its children when it exits, and a process that
;; took a minute to boot should still be there when Emacs comes back.
;;
;; That buffer matters more than it looks: what reaches it is everything the
;; process printed that belongs to no repl - a background thread, a logging
;; framework, a library that writes to System.out from the very form you just
;; evaluated.  A developer who cannot find it will think that output vanished.
;;
;; The pipe goes both ways, and the other direction is the standard input of
;; the jvm - see `replique-process-input'.  What reads there is not a repl:
;; it is java.io.Console, and what it asks for is asked before there is a
;; repl to answer with, a keystore passphrase being the usual one.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'replique-common)
(require 'replique-edn)
(require 'replique-conn)
(require 'replique-exception)

(defcustom replique-clojure-program "clojure"
  "The clojure command used to start a process."
  :type 'string
  :group 'replique)

(defcustom replique-coordinates nil
  "The tools.deps coordinate of replique itself, as EDN.

Replique is a tool the editor brings, not something a project should have
to depend on: it would otherwise be in the dependencies of everyone
working on that project, whether they use this or not, and in what the
project builds.  So the editor puts it on the classpath itself, and a
project needs no change to be worked on.

Nil leaves it out, for a project that does depend on replique.

Risky, like everything here that builds the command line: what it names
is put on the classpath of the process, so it is not a setting a project
being opened is allowed to offer quietly."
  :type '(choice (const :tag "The project provides replique" nil) string)
  :risky t
  :group 'replique)

(defcustom replique-aliases nil
  "Aliases of the deps.edn of the project to start the process with.

A project whose sources are behind an alias needs it to be usable at all.
Which aliases those are is a property of the project rather than of you,
so .dir-locals.el is where this belongs - and it is read from the project
being started, whatever buffer the command was called from."
  :type '(repeat string)
  :safe (lambda (value) (and (listp value) (seq-every-p #'stringp value)))
  :group 'replique)

(defcustom replique-aliases-file ".replique/aliases.edn"
  "Where in a project the aliases that are yours are defined, if any.

Tooling that is yours but belongs to one project has nowhere good to go:
the deps.edn of the project is shared with everyone working on it, and
~/.clojure/deps.edn is not about this project.  This file is - it sits in
the project, it is meant to be ignored by its version control, and what it
holds is a map of aliases:

  {:mine {:extra-deps {org.clojure/data.json {:mvn/version \"2.5.1\"}}
          :extra-paths [\"dev-local\"]}}

They are passed as the aliases of -Sdeps, which is merged as the last deps
file - so the deps.edn of the project and your own are both still in
effect, nothing is replaced.  Name the ones to use in `replique-aliases',
from .dir-locals-2.el, which is where Emacs keeps what is yours rather
than the project's.

The text is passed on as it was written.  Replique does not read it, which
is why the aliases it defines have to be named rather than found - and
why the name of the file is risky: whatever it holds reaches -Sdeps
unread, so a project that could point this somewhere of its own would be
choosing what the process runs with."
  :type 'string
  :risky t
  :group 'replique)

(defcustom replique-user-aliases nil
  "Aliases of your own deps.edn to start every process with.

Tooling that is yours rather than the project's - a debugger, a profiler,
whatever you like working with - goes in the aliases of ~/.clojure/deps.edn
and is named here.  Nothing about it reaches the project, so nobody you
work with has to know it is there, and no file of theirs has to change.

These are added to `replique-aliases', they do not replace them: a project
that needs an alias to be usable still gets it.  Set this in your init
file - a project must not be able to choose what you run, which is what
the global value being the one that is read comes to: see
`replique-process--main-opt'."
  :type '(repeat string)
  :risky t
  :group 'replique)

(cl-defstruct (replique-process
               (:constructor replique-process--make)
               (:conc-name replique-process--))
  "A replique process.

INFO is the description the process gives of itself - the same map it
writes to its port file.  PROC is the operating system process when Emacs
started it, nil when Emacs only connected to it.

DIRECTORY is where it runs, named the way the editor names it, which is
not always the way the process does - see \"The name a path comes back
under\" below.  RENAMING is how to write one of its paths the other way,
and what says the two names are different at all."
  id host port directory renaming info proc control repls output-buffer)

(defvar replique-process-started-hook nil
  "Functions called with a process once it is connected.")

(defvar replique-processes nil
  "The replique processes this Emacs is connected to.")

(defvar replique-current-process nil
  "The process the commands act on.")

;;; The process registry

(defun replique-process-live-p (process)
  "Return non-nil when PROCESS still has a control connection."
  (and process (replique-conn-live-p (replique-process--control process))))

(defun replique-processes-live ()
  "Return the processes that are still connected."
  (seq-filter #'replique-process-live-p replique-processes))

(defun replique-process-connected (info)
  "Return the process INFO describes, when Emacs is already connected to it.

Id, host and port together: an id names a directory, and two checkouts of
one project have the same one, while a port on its own can have been taken
over by another process since the port file was written."
  (seq-find (lambda (process)
              (and (equal (replique-process--id process) (plist-get info :process-id))
                   (equal (replique-process--host process) (plist-get info :host))
                   (equal (replique-process--port process) (plist-get info :port))))
            (replique-processes-live)))

(defun replique-process-in (directory)
  "Return the live process Emacs has in DIRECTORY, if any.

Only what this Emacs is connected to.  A process started in a terminal is
not known here, and nothing short of connecting to it would say whether
the port file it left names something that is still running.

One directory reached two ways is one directory.  A project opened through
a symlink and the same project opened as what that link resolves to are
one place with two names, and a process there is one process however it
was named: what would come of starting a second is the port file of the
first one refusing it, from somewhere the message does not point at."
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    (seq-find (lambda (process)
                (when-let* ((dir (replique-process--directory process)))
                  (let ((dir (file-name-as-directory (expand-file-name dir))))
                    (or (equal directory dir)
                        (file-equal-p directory dir)))))
              (replique-processes-live))))

(defun replique-process--register (process)
  "Remember PROCESS and make it the current one."
  (setq replique-processes (cons process replique-processes))
  (setq replique-current-process process)
  process)

(defun replique-process--forget (process)
  "Drop PROCESS from the registry."
  (setq replique-processes (delq process replique-processes))
  (when (eq replique-current-process process)
    (setq replique-current-process (car (replique-processes-live)))))

(defun replique-process-current ()
  "Return the process the commands act on, or nil.

Falls back to the only live one, which is what there usually is."
  (cond
   ((replique-process-live-p replique-current-process) replique-current-process)
   (t (setq replique-current-process (car (replique-processes-live))))))

(defun replique-process-ensure ()
  "Return the process the commands act on, or signal that there is none."
  (or (replique-process-current)
      (user-error "No replique process - M-x replique-start or M-x replique-connect")))

;;; The output of a process

(define-derived-mode replique-process-mode special-mode "Replique-Process"
  "Major mode for what a process printed outside of any repl.

No font lock: what goes in carries the faces it needs, and font lock would
paint over them."
  (setq-local truncate-lines nil))

(defun replique-process-buffer (process)
  "Return the buffer holding what PROCESS printed outside of any repl."
  (let ((buffer (replique-process--output-buffer process)))
    (unless (buffer-live-p buffer)
      (setq buffer (generate-new-buffer
                    (format "*replique-process: %s*" (replique-process--id process))))
      (with-current-buffer buffer (replique-process-mode))
      (setf (replique-process--output-buffer process) buffer))
    buffer))

(defun replique-process--insert (process string &optional face)
  "Show STRING in the output buffer of PROCESS, with FACE."
  (replique-insert-output (replique-process-buffer process) string face))

(defun replique-process--note (process format &rest args)
  "Say something about PROCESS in its output buffer, from FORMAT and ARGS."
  (replique-process--insert process
                            (concat (apply #'format format args) "\n")
                            'replique-note))

;;; The name a path comes back under

;; A process resolves the directory it runs in and the editor does not.  The
;; kernel resolves every link on the way to a working directory and `user.dir'
;; is what that resolving left, while the editor holds the name somebody
;; opened.  Where a project is reached through a symlink those are two names
;; for one directory, and every path the process hands back is written its way:
;; a definition in ~/clojure/repl/src/app.clj is answered as
;; ~/clojure/worktree-a/src/app.clj, and opening it takes the buffer out of the
;; link.
;;
;; Which matters because the link is what says what is being edited.  Pointing
;; one at another worktree is how a branch is switched under a running process,
;; and a buffer on the far side of it says nothing about which branch its file
;; is being edited for - version control answers for the resolved tree, and
;; replique itself would key a process to it.
;;
;; So a path is renamed as it comes in, once, for every answer alike: an editor
;; that asked about ~/clojure/repl is told about ~/clojure/repl.  Only the
;; prefix moves.  What the process says about a file is otherwise what it says,
;; and a path outside the directory it runs in - a jar under ~/.m2, a source
;; beside it - is named the one way both of them have for it.
;;
;; Neither of the two names is itself renamed.  The process's own is kept in
;; its INFO, which is where the rename is read out of, and is what
;; `replique-describe-process' reports beside the other: the resolved path is
;; the one that says which worktree a link is pointing at, and a process is
;; entitled to say where it is.
;;
;; What is renamed is every reply, which is every answer there is: a path comes
;; back as the `:file' of a definition, of a use of a name, and of a file that
;; has to be loaded again, and each of those is a reply to something asked.  No
;; frame a process pushes carries one - what arrives unasked for is output, an
;; exception in a thread, and a count of what was dropped - so
;; `replique-process--frame' renames nothing, and is where to rename it if one
;; ever does.  Neither does a repl connection carry a path: what comes back
;; there is a prompt, output, a value, and an exception written out.

(defun replique-process--renaming-between (editor process)
  "Return how to write a path of PROCESS the way EDITOR writes it, or nil.

EDITOR and PROCESS are two names for the directory a process runs in: the
one the editor was given, and the one the process resolved.  The rule is
the two of them as prefixes, to swap one for the other.

Nil where there is nothing to swap - the same name twice - and nil where
they are not one directory at all, which is a process running somewhere
else and nothing to rename the paths of."
  (when (and editor process)
    (let ((editor (file-name-as-directory (expand-file-name editor)))
          (process (file-name-as-directory (expand-file-name process))))
      (when (and (not (equal editor process))
                 (file-equal-p editor process))
        (cons process editor)))))

(defun replique-process--renamed-path (path renaming)
  "Return PATH written the way RENAMING says, or PATH where it says nothing.

Only a path under the directory the rule is about is renamed, and only its
prefix: everything below is what the process said."
  (if (and renaming (stringp path) (string-prefix-p (car renaming) path))
      (concat (cdr renaming) (substring path (length (car renaming))))
    path))

(defun replique-process--renamed (value renaming)
  "Return VALUE with every file in it written the way RENAMING says.

VALUE is a frame, or anything inside one: a property list, a list of them,
or something that is neither and comes back as it is.

A path is the value of a `:file', wherever one is, which is what a file is
called throughout the protocol.  Nothing else is touched.  An `:entry'
beside a `:file' names something inside an archive rather than a place on
a disk, and a `:directory' is a process saying where it is - the thing the
renaming is made of."
  (cond
   ((not (consp value)) value)
   ((keywordp (car value))
    (let ((renamed nil))
      (while value
        (let ((key (car value))
              (each (cadr value)))
          (push key renamed)
          (push (if (eq key :file)
                    (replique-process--renamed-path each renaming)
                  (replique-process--renamed each renaming))
                renamed)
          (setq value (cddr value))))
      (nreverse renamed)))
   (t (mapcar (lambda (each) (replique-process--renamed each renaming)) value))))

(defun replique-process--renamed-frame (process frame)
  "Return FRAME with every file in it named the way PROCESS is named here.

Which is FRAME itself where the editor and the process have one name for
the directory, and that is nearly always."
  (let ((renaming (replique-process--renaming process)))
    (if renaming
        (replique-process--renamed frame renaming)
      frame)))

;;; Events

(defun replique-process--event (process frame)
  "Handle FRAME, an unsolicited frame from the control connection of PROCESS."
  (pcase (plist-get frame :event)
    ("out" (replique-process--insert process (plist-get frame :string)))
    ("err" (replique-process--insert process (plist-get frame :string) 'replique-stderr))
    ("uncaught-exception"
     (let ((thread (plist-get frame :thread))
           (message (plist-get frame :message))
           (exception (plist-get frame :exception)))
       (replique-process--insert
        process
        (replique-exception-button
         (format "Exception in thread \"%s\" %s\n" thread message)
         exception message nil (format "in thread %s" thread))
        'replique-stderr)
       (message "replique: exception in thread \"%s\": %s" thread message)))
    ("dropped"
     ;; Written where the gap is: everything that survived came before it
     (replique-process--note process "... %s events were dropped ..."
                             (plist-get frame :count)))
    (_ nil)))

(defun replique-process--frame (process frame)
  "Handle FRAME on the control connection of PROCESS."
  (pcase (plist-get frame :tag)
    ("event" (replique-process--event process frame))
    ("error" (message "replique: %s: %s"
                      (plist-get frame :error) (plist-get frame :message)))
    (_ nil)))

;;; Connecting

(defun replique-processes-directory (directory)
  "Return the directory holding the description of the processes in DIRECTORY."
  (expand-file-name ".replique/processes/" directory))

(defun replique-process--description (file)
  "Return what the process that wrote FILE said about itself, or nil.

Nil for a file that cannot be read or does not hold a description: what is
in that directory is what processes put there, and a file that says
nothing is not one to act on."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file)
        (json-parse-string (buffer-string)
                           :object-type 'plist
                           :array-type 'list
                           :null-object nil
                           :false-object nil))
    (error nil)))

(defun replique-process-descriptions (directory)
  "Return the processes that say they are running in DIRECTORY.

Each is a cons of the port file and what the process wrote in it.  A stale
file - one whose process is gone - is not told apart here: connecting is
the only thing that says whether anything is there, and
`replique-process--reap' is what acts on the answer."
  (let ((dir (replique-processes-directory directory)))
    (when (file-directory-p dir)
      (seq-keep
       (lambda (file)
         (when-let* ((info (replique-process--description file)))
           (cons file info)))
       (directory-files dir t "\\.json\\'")))))

(defun replique-process--loopback-p (host)
  "Return non-nil when HOST is this machine and can be no other."
  (member host '("127.0.0.1" "::1" "localhost")))

(defun replique-process--reap (file info reason)
  "Delete FILE, the port file that said INFO, when REASON proves it wrong.

A port file is deleted on evidence and on nothing else.  `mismatch' is an
answer from a process that is not the one the file names, which says the
file is wrong wherever that process runs.  `unreachable' is nothing
answering at all, which says the same thing only when the file names this
machine: a host that is somewhere else can be unreachable for reasons of
its own, and a process that is alive must not lose the file that makes it
findable.

Read again before deleting: a process that died and started again between
the read and the connect wrote a file of its own, and that one is about a
process that is there.  It is what the file says now, not what it said,
that has to be wrong."
  (when (and (or (eq reason 'mismatch)
                 (replique-process--loopback-p (plist-get info :host)))
             (file-exists-p file))
    (let ((current (replique-process--description file)))
      (when (and current
                 (equal (plist-get current :pid) (plist-get info :pid))
                 (equal (plist-get current :started-at) (plist-get info :started-at)))
        ;; A file that will not go is not a reason for a command to fail: it
        ;; says something wrong about a directory, and that is all
        (ignore-errors (delete-file file))))))

(defun replique-process--listening-p (host port)
  "Return non-nil when something accepts a connection on HOST and PORT.

Whether what answers is a replique process, let alone the one that was
expected, is not asked here - a handshake is what settles that, and it is
an answer that comes later.  This says only that the port is not dead."
  (let ((proc (condition-case nil
                  (make-network-process :name "replique-probe"
                                        :host host :service port :noquery t)
                (file-error nil))))
    (when proc
      (delete-process proc)
      t)))

(defun replique-process--reap-directory (directory)
  "Delete the port files of DIRECTORY that nothing answers for.

What a start does about the file a crash left behind: a process that was
killed outright never ran the hook that deletes its port file, so the file
goes on naming it - and a directory that has a port file is one the
process refuses to start in.  Reaped here rather than left for the first
connect, so that the way back from a crash is the command that was going
to be typed anyway.

Only a port on this machine is probed, and only nothing listening is taken
as an answer.  A port that answers, with a handshake or with a refusal, is
a slower question, and `replique-connect' is where it is asked."
  (dolist (description (replique-process-descriptions directory))
    (let* ((info (cdr description))
           (host (plist-get info :host))
           (port (plist-get info :port)))
      (when (and port
                 (replique-process--loopback-p host)
                 (not (replique-process--listening-p host port)))
        (replique-process--reap (car description) info 'unreachable)))))

(defun replique-process--connect (info directory os-proc on-ready &optional on-failure)
  "Open the control connection of the process described by INFO.

DIRECTORY is the name the editor has for where the process runs - what was
given to `replique-start' or `replique-connect' - and nil where there is
none.  It is what the process is known by here, and what every path it
hands back is renamed under, where the process resolved that name into
another one: see \"The name a path comes back under\".

OS-PROC is the operating system process when Emacs started it.  ON-READY
is called with the replique process once the handshake is in.  Returns the
process, or nil when there was nothing to connect to.

ON-FAILURE is called with why the connection was not made and a sentence
saying it: `unreachable' when nothing answered on that port, `mismatch'
when what answered is not the process INFO describes.  Both are what a
port file produces once it is old enough - the process it names has
exited, or has exited and left its port to somebody else - and a caller
that read INFO from one has a file to do something about.  Said in the
echo area when there is no ON-FAILURE.

The two arrive differently: nothing to connect to is known before this
returns, since the connection is made before `replique-conn-open'
returns, while a refused handshake is an answer that comes later."
  (let* ((host (plist-get info :host))
         (port (plist-get info :port))
         (renaming (replique-process--renaming-between
                    directory (plist-get info :directory)))
         (process (replique-process--make
                   :id (plist-get info :process-id)
                   :host host
                   :port port
                   ;; The editor's name for it exactly where that is a name
                   ;; for the same directory, which is what having a rule
                   ;; says.  Without one there is nothing to choose between
                   ;; and the process's own name stands
                   :directory (if renaming (cdr renaming) (plist-get info :directory))
                   :renaming renaming
                   :info info
                   :proc os-proc
                   :repls nil))
         (fail (lambda (reason why)
                 (if on-failure
                     (funcall on-failure reason why)
                   (message "replique: %s" why))))
         (conn (condition-case err
                   (replique-conn-open
                    host port 'control
                    ;; Against a stale port file: the process answering on
                    ;; that port may not be the one the file describes
                    :process-id (plist-get info :process-id)
                    :on-frame (lambda (frame) (replique-process--frame process frame))
                    :on-close (lambda (_conn)
                                ;; A process that never got connected has
                                ;; nothing to say and no buffer to say it in
                                (when (memq process replique-processes)
                                  (replique-process--note process "The process is gone"))
                                (replique-process--forget process))
                    :on-error (lambda (frame)
                                (funcall fail 'mismatch
                                         (format "%s:%s is not %s any more - %s"
                                                 host port (plist-get info :process-id)
                                                 (plist-get frame :message))))
                    :on-ready (lambda (_conn)
                                (replique-process--register process)
                                (when on-ready (funcall on-ready process))))
                 (file-error
                  ;; The reason and not the whole error: what
                  ;; `error-message-string' makes of one holds every
                  ;; argument the connection was attempted with
                  (let ((reason (nth 2 err)))
                    (funcall fail 'unreachable
                             (if (stringp reason)
                                 (format "Nothing is listening on %s:%s - %s"
                                         host port reason)
                               (format "Nothing is listening on %s:%s" host port))))
                  nil))))
    (when conn
      (setf (replique-process--control process) conn)
      process)))

;;; The directory a command is about

(defun replique-process--dominating (predicate)
  "Return the nearest directory at or above the buffer PREDICATE accepts."
  (when-let* ((directory (locate-dominating-file default-directory predicate)))
    ;; `locate-dominating-file' abbreviates what it returns, and a name with
    ;; a ~ in it is not one to compare against a directory or to pass on
    (file-name-as-directory (expand-file-name directory))))

(defun replique-process--project-root ()
  "Return the project of the current buffer, as somewhere to start looking.

The nearest deps.edn above the buffer: it is what `replique-clojure-program'
reads, so it is what makes a directory one a process can run in, and the
nearest of them is the module rather than the repository holding it.
Failing that, whatever Emacs itself calls the project - a version control
root, usually, which is where a deps.edn is not, so it is a guess and not
an answer.  Failing that too, where the buffer is."
  (or (replique-process--dominating
       (lambda (directory) (file-exists-p (expand-file-name "deps.edn" directory))))
      (when-let* ((project (project-current)))
        (file-name-as-directory (expand-file-name (project-root project))))
      default-directory))

(defun replique-process--directory-to-start ()
  "Return the directory `replique-start' proposes.

The nearest project above the buffer that has no process of Emacs' own.
A start is asked for from the buffers of a project already being worked
on, a repl among them - and a repl buffer is in the directory of its own
process, which is the one directory `replique-start' refuses.  When every
project above the buffer is taken, the nearest is proposed all the same:
what a command proposes is where completion starts, not what it does."
  (or (replique-process--dominating
       (lambda (directory)
         (and (file-exists-p (expand-file-name "deps.edn" directory))
              (null (replique-process-in directory)))))
      (replique-process--project-root)))

(defun replique-process--directory-to-connect ()
  "Return the directory `replique-connect' proposes.

The nearest directory above the buffer that a process says it is running
in.  A port file is the whole of what `replique-connect' needs, so a
directory that has one is an answer rather than a guess - and it is not
the project root of anything: it is the directory a process was started
in, which is only usually the same thing."
  (or (replique-process--dominating #'replique-process-descriptions)
      (replique-process--project-root)))

;;; Starting

(defun replique-process--id-for (directory)
  "Return a process id naming DIRECTORY, or nil when its name cannot be one.

A process id is used as a file name, so the process refuses anything that
would not make one."
  (let ((name (file-name-nondirectory (directory-file-name directory))))
    (when (string-match-p "\\`[A-Za-z0-9][A-Za-z0-9._+-]\\{0,127\\}\\'" name)
      name)))

(defun replique-process--local-aliases (directory)
  "Return the aliases of your own kept in DIRECTORY, as the text of a map."
  (let ((file (expand-file-name replique-aliases-file directory)))
    (when (file-readable-p file)
      (let ((text (string-trim (with-temp-buffer
                                 (insert-file-contents file)
                                 (buffer-string)))))
        (unless (string-empty-p text) text)))))

(defun replique-process--sdeps (directory)
  "Return the deps data a process started in DIRECTORY is given, or nil."
  (let ((aliases (replique-process--local-aliases directory)))
    (when (or replique-coordinates aliases)
      (concat "{"
              (when replique-coordinates
                (format ":deps {replique/replique %s}" replique-coordinates))
              (when aliases
                (concat (when replique-coordinates " ") ":aliases " aliases))
              ;; The brace goes on a line of its own: the aliases were spliced
              ;; in as they were written, and a comment on their last line
              ;; would otherwise swallow it
              "\n}"))))

(defun replique-process--project-aliases (directory)
  "Return the aliases DIRECTORY asks for.

The directory local variables of the project rather than of the current
buffer: a process is started for a project, and the buffer that asked for
it may be anywhere - a repl of another project, or no file at all.

Which is why the global value is where this starts, and not the value the
calling buffer has: a buffer visiting a file in one project carries that
project's aliases, and they are not the ones the project being started
asked for.  What is yours rather than a project's goes in
`replique-user-aliases'."
  (let ((aliases (default-value 'replique-aliases)))
    (with-temp-buffer
      (setq-local replique-aliases aliases)
      (setq-local default-directory directory)
      (hack-dir-local-variables-non-file-buffer)
      replique-aliases)))

(defun replique-process--main-opt ()
  "Return the main option, under the aliases the process should run with.

What the project asks for and what you asked for, in that order: yours
last, so that yours is what wins where they say the same thing.

Yours are the global value and not the value the calling buffer has, for
the reason `replique-process--project-aliases' reads the global one: a
buffer visiting a file in one project carries that project's directory
local variables, and a start is for the project that was named.  A buffer
local value here would not add to yours - it would be read instead of
them, and the tooling you start every process with would go missing from
the one process it was set in the way of."
  (let ((aliases (delete-dups
                  (mapcar (lambda (alias) (concat ":" (string-remove-prefix ":" alias)))
                          (append replique-aliases
                                  (default-value 'replique-user-aliases))))))
    (if aliases
        (concat "-M" (string-join aliases))
      "-M")))

(defun replique-process--command (directory)
  "Return the command starting a replique process in DIRECTORY.

Under nohup where there is one: Emacs sends SIGHUP to what it started when
it exits, and a process that is meant to be connected to again has to live
through that.  Where there is none the process is Emacs's to lose."
  (let ((id (replique-process--id-for directory)))
    (append (when (executable-find "nohup") (list "nohup"))
            (list replique-clojure-program)
            ;; -Sdeps is merged as the last deps file rather than replacing
            ;; any of them, so bringing replique along, and whatever else is
            ;; yours, costs the project nothing
            (when-let* ((sdeps (replique-process--sdeps directory)))
              (list "-Sdeps" sdeps))
            (list (replique-process--main-opt) "-m" "replique.main")
            (when id (list (replique-edn-map (list :process-id id)))))))

(defun replique-process--startup-buffer (proc)
  "Return the buffer holding what PROC wrote."
  (process-get proc 'replique-buffer))

(defun replique-process--failed (proc why &optional exception summary)
  "Say once that PROC is not a replique process, because of WHY.

Once rather than per line: what a jvm that will not start writes is a
stack trace, and a stack trace reported one line at a time in the echo
area is how a message stops being read.  EXCEPTION, when the process got
far enough to send one, is written as a way into the whole of it -
SUMMARY being what it would have been reported as."
  (unless (eq 'failed (process-get proc 'replique-state))
    (process-put proc 'replique-state 'failed)
    (let ((buffer (replique-process--startup-buffer proc)))
      ;; WHY is said in the echo area and nowhere else: the buffer holds what
      ;; the process wrote, and a line of replique's own in the middle of a
      ;; stack trace is a line the process did not write
      (when exception
        (replique-insert-output
         buffer
         (replique-exception-button "browse the exception\n" exception summary nil
                                    "starting the process")
         'replique-note))
      ;; The buffer is where the whole of it is, and it can have been killed
      ;; while the process was starting.  Then there is nowhere to point at,
      ;; and what is left to do is say what happened
      (if (buffer-live-p buffer)
          (progn
            (message "replique: %s - see %s" why (buffer-name buffer))
            (display-buffer buffer))
        (message "replique: %s" why)))))

(defun replique-process--spawn-filter (proc string)
  "Read the startup line PROC wrote in STRING, then let its output through.

The startup line is protocol: a client reads it to find the process it
just started.  Anything else means the process is not one, and from there
what it writes is a diagnostic - which belongs in a buffer, whole, rather
than in the echo area a line at a time."
  (if (eq 'starting (process-get proc 'replique-state))
      (let* ((acc (concat (or (process-get proc 'replique-acc) "") string))
             (idx (string-search "\n" acc)))
        (if (not idx)
            (process-put proc 'replique-acc acc)
          (process-put proc 'replique-acc nil)
          (replique-process--started proc (substring acc 0 idx))
          (let ((rest (substring acc (1+ idx))))
            (unless (string-empty-p rest)
              (replique-process--spawn-filter proc rest)))))
    (replique-process--wrote proc string)))

(defun replique-process--wrote (proc string)
  "Show STRING, which PROC wrote, in the output buffer PROC belongs to.

Only until the control connection is up.  What the process prints goes
both to the pipe and to the connections, so from there the two say the
same thing - and the connection is the one that keeps saying it when Emacs
is not the one holding the pipe.  What comes before is the pipe alone: the
jvm, the clojure script, and anything printed while there was nothing to
be an event on.

Read to the end whatever is done with it - a pipe nobody reads fills up,
and the process stops on the write that fills it."
  (unless (eq 'connected (process-get proc 'replique-state))
    (let ((process (process-get proc 'replique-process)))
      (if process
          (replique-process--insert process string)
        (replique-insert-output (replique-process--startup-buffer proc) string)))))

(defun replique-process--adopt-buffer (process proc)
  "Give PROCESS the buffer the startup output of PROC went to."
  (let ((buffer (replique-process--startup-buffer proc)))
    (when (buffer-live-p buffer)
      (setf (replique-process--output-buffer process) buffer)
      (with-current-buffer buffer
        (rename-buffer (format "*replique-process: %s*"
                               (replique-process--id process))
                       t)))))

(defun replique-process--started (proc line)
  "Act on LINE, the startup line PROC wrote."
  (let ((info (condition-case nil
                  (json-parse-string line
                                     :object-type 'plist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)
                (error nil))))
    (cond
     ((equal "started" (plist-get info :tag))
      (process-put proc 'replique-state 'started)
      (let ((process (replique-process--connect
                      info (process-get proc 'replique-directory) proc
                      (lambda (process)
                        (process-put proc 'replique-state 'connected)
                        (message "replique: %s listening on %s:%s"
                                 (replique-process--id process)
                                 (replique-process--host process)
                                 (replique-process--port process))
                        (run-hook-with-args 'replique-process-started-hook process))
                      ;; The process said where it was listening and is not
                      ;; there.  Nothing was read from a file here, so there
                      ;; is nothing to clean up - what is left is to say it
                      ;; where the rest of the start is reported
                      (lambda (_reason why)
                        (replique-process--failed proc why)))))
        (when process
          (process-put proc 'replique-process process)
          ;; The buffer the startup output went to becomes the buffer of the
          ;; process: what it printed before it was up belongs with the rest
          (replique-process--adopt-buffer process proc))))
     ((equal "error" (plist-get info :tag))
      (replique-process--failed
       proc
       (format "The process could not start: %s" (plist-get info :message))
       (plist-get info :exception)
       (plist-get info :message)))
     (t
      ;; Not protocol at all.  The line belongs in the buffer with whatever
      ;; else the process is about to say
      (replique-insert-output (replique-process--startup-buffer proc)
                              (concat line "\n"))
      (replique-process--failed proc "The process did not announce itself")))))

(defun replique-process--spawn-sentinel (proc _event)
  "Notice that the operating system process PROC is gone."
  (unless (process-live-p proc)
    (let ((process (process-get proc 'replique-process)))
      (if process
          (replique-process--note process "The process exited")
        (let ((buffer (replique-process--startup-buffer proc)))
          (replique-insert-output
           buffer
           (format "\nThe process exited with status %s\n" (process-exit-status proc))
           'replique-note)
          (replique-process--link-report buffer)
          (if (buffer-live-p buffer)
              (message "replique: the process exited without starting - see %s"
                       (buffer-name buffer))
            (message "replique: the process exited without starting")))))))

(defun replique-process--link-report (buffer)
  "Turn the report clojure wrote, named in BUFFER, into a file to open."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^Full report at:\n\\(.*\\)$" nil t)
          (let ((inhibit-read-only t))
            (make-text-button
             (match-beginning 1) (match-end 1)
             'action (lambda (button) (find-file (button-label button)))
             'help-echo "RET: open the report clojure wrote")))))))

;;;###autoload
(defun replique-start (directory)
  "Start a replique process in DIRECTORY and connect to it.

The project needs no change to be worked on: `replique-coordinates' is
put on the classpath alongside its own dependencies, together with
whatever `replique-aliases-file' defines, under the aliases
`replique-aliases' and `replique-user-aliases' name.

A directory that already has a process is refused - see
`replique-kill-process'.  The port file of a process that is gone is
deleted first: a crash leaves one behind, and the process will not start
where there is one."
  (interactive (list (read-directory-name "Project directory: "
                                          (replique-process--directory-to-start)
                                          nil t)))
  (unless (executable-find replique-clojure-program)
    (user-error "No %s on exec-path - see replique-clojure-program"
                replique-clojure-program))
  (let ((directory (file-name-as-directory (expand-file-name directory))))
    ;; Before anything is spawned or any buffer made.  A process names its
    ;; port file after its directory, so a second process there would take
    ;; the file of the first - which would leave the first unreachable to
    ;; anything that connects, and the two of them one id the commands
    ;; cannot tell apart
    (when-let* ((running (replique-process-in directory)))
      (user-error "%s is already running in %s - M-x replique-kill-process to start another"
                  (replique-process--id running) directory))
    ;; After that guard and before the spawn: what is reaped here is a file
    ;; naming a process that is not there, and a process Emacs holds is
    ;; there whatever its file says
    (replique-process--reap-directory directory)
    (let* ((default-directory directory)
           (replique-aliases (replique-process--project-aliases directory))
           (command (replique-process--command directory))
           (buffer (generate-new-buffer
                    (format "*replique-process: %s*"
                            (file-name-nondirectory (directory-file-name directory))))))
      (with-current-buffer buffer (replique-process-mode))
      (message "replique: %s" (string-join command " "))
      (let ((proc (make-process
                   :name "replique"
                   :buffer nil
                   :command command
                   :coding 'utf-8-unix
                   :connection-type 'pipe
                   :noquery t
                   :filter #'replique-process--spawn-filter
                   :sentinel #'replique-process--spawn-sentinel)))
        (process-put proc 'replique-buffer buffer)
        (process-put proc 'replique-state 'starting)
        (process-put proc 'replique-directory directory)
        proc))))

;;;###autoload
(defun replique-connect (directory)
  "Connect to a replique process already running in DIRECTORY."
  (interactive (list (read-directory-name "Project directory: "
                                          (replique-process--directory-to-connect)
                                          nil t)))
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (descriptions (replique-process-descriptions directory)))
    (cond
     ((null descriptions)
      (user-error "No process is running in %s" directory))
     (t
      (let* ((choices (mapcar (lambda (description)
                                (let ((info (cdr description)))
                                  (cons (format "%s (%s:%s)"
                                                (plist-get info :process-id)
                                                (plist-get info :host)
                                                (plist-get info :port))
                                        description)))
                              descriptions))
             (description (if (cdr choices)
                              (cdr (assoc (completing-read "Process: " choices nil t)
                                          choices))
                            (cdar choices)))
             (file (car description))
             (info (cdr description))
             (connected (replique-process-connected info)))
        (if connected
            ;; Connecting again to what Emacs is already connected to is
            ;; asking for the process, not for a second connection to it.
            ;; The second would be a poor one: a process Emacs started is
            ;; asked not to tee, because Emacs reads its pipe, so what it
            ;; prints would never reach that connection - and two processes
            ;; of one id are two entries the commands cannot tell apart
            (progn
              (setq replique-current-process connected)
              (message "replique: already connected to %s"
                       (replique-process--id connected)))
          (replique-process--connect
           info directory nil
           (lambda (process)
             (replique-process-buffer process)
             (message "replique: connected to %s" (replique-process--id process))
             (run-hook-with-args 'replique-process-started-hook process))
           ;; The file said where a process was and it is not there.  It is
           ;; deleted rather than left: it would go on being offered here,
           ;; and the directory is what says what is running
           (lambda (reason why)
             (replique-process--reap file info reason)
             (message "replique: %s" why)))))))))

;;; Ops

(defun replique-process-request (process msg &optional callback)
  "Send MSG on the control connection of PROCESS.

CALLBACK is called with the reply, every file in it named the way this
process is named here - see \"The name a path comes back under\"."
  (let ((conn (replique-process--control process)))
    (unless (replique-conn-live-p conn)
      (user-error "The process is not connected"))
    (replique-conn-request
     conn msg
     (when callback
       (lambda (frame)
         (funcall callback (replique-process--renamed-frame process frame)))))))

(defun replique-process-request-sync (process msg &optional timeout)
  "Send MSG on the control connection of PROCESS and wait for the reply.

TIMEOUT is passed on to `replique-conn-request-sync', which is where what
is returned is described.

Where `replique-process-request' signals that there is no connection,
this answers with the frame that says so.  What waits for a reply is a
keystroke: a command that raises in the middle of one stops the editor
where offering nothing would have let the typing go on."
  (replique-process--renamed-frame
   process
   (replique-conn-request-sync (replique-process--control process) msg timeout)))

(defun replique-describe-process ()
  "Say what the current process is."
  (interactive)
  (let ((process (replique-process-ensure)))
    (replique-process-request
     process (list :op :process-info)
     (lambda (frame)
       (if (equal "error" (plist-get frame :tag))
           (message "replique: %s" (plist-get frame :message))
         (message "replique: %s in %s - clojure %s, java %s, up %ss"
                  (plist-get frame :process-id)
                  ;; Both names where there are two: this is the one command
                  ;; that is asking what the process is, and where a link is
                  ;; what the editor reached it through, what it resolved to
                  ;; is what says which worktree is under the link now
                  (if (replique-process--renaming process)
                      (format "%s -> %s"
                              (replique-process--directory process)
                              (plist-get frame :directory))
                    (plist-get frame :directory))
                  (plist-get frame :clojure-version)
                  (plist-get frame :java-version)
                  (/ (or (plist-get frame :uptime) 0) 1000)))))))

;;;###autoload
(defun replique-select-process ()
  "Choose the process the commands act on."
  (interactive)
  (let ((processes (replique-processes-live)))
    (unless processes (user-error "No replique process"))
    (let* ((choices (mapcar (lambda (p) (cons (replique-process--id p) p)) processes))
           (choice (completing-read "Process: " choices nil t)))
      (setq replique-current-process (cdr (assoc choice choices)))
      (message "replique: %s" choice))))

(defun replique-show-process-output ()
  "Show what the current process printed outside of any repl."
  (interactive)
  (pop-to-buffer (replique-process-buffer (replique-process-ensure))))

;;; The standard input of the process

(defun replique-process--stdin (process)
  "Return the operating system process of PROCESS, to write to.

A repl reads what is typed at its prompt, and that is a socket.  The
standard input of the jvm is a different thing entirely, and it is what
`java.io.Console' reads - a keystore passphrase asked for at startup, an
agent asking something before any repl exists.  Nothing of that reaches a
repl, and nothing typed at a repl reaches it."
  (let ((proc (replique-process--proc process)))
    (cond
     ((null proc)
      (user-error "Emacs did not start this process - its input is not here"))
     ((not (process-live-p proc))
      (user-error "The process is gone"))
     (t proc))))

(defun replique-process-input (line)
  "Send LINE to the standard input of the current process.

What the jvm reads there, and what it asks for there, is not what a repl
reads - see `replique-process--stdin'.  What it prints in answer is in
the process buffer, which \[replique-show-process-output] shows."
  (interactive (list (read-string "Process input: ")))
  (process-send-string (replique-process--stdin (replique-process-ensure))
                       (concat line "\n")))

(defun replique-process-input-password (password)
  "Send PASSWORD to the standard input of the current process, unechoed.

The same as `replique-process-input', asked for in a way that does not
show it, does not keep it in the minibuffer history, and does not leave
it where \[view-lossage] can be asked for it.  A command of its own
rather than an argument to that one: a password echoed because the
argument was forgotten is a password that has already been echoed."
  (interactive (list (read-passwd "Process input: ")))
  (process-send-string (replique-process--stdin (replique-process-ensure))
                       (concat password "\n")))

(provide 'replique-process)

;;; replique-process.el ends here
