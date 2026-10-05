;;; replique-classpath.el --- Whether the classpath is still the right one  -*- lexical-binding: t; -*-

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

;; A classpath is computed once, when the process starts, and the files it
;; was computed from go on changing: deps.edn is edited, a branch with other
;; dependencies is checked out, a directory the process reads through a link
;; is pointed at another worktree.  Nothing about a running process says so -
;; it goes on answering with what it had, and the first sign is a require
;; that cannot find a namespace, or one that finds the old one.
;;
;; SO THIS ASKS, AND IT ASKS IN THREE STEPS OF RISING COST.  Each is asked
;; only where the one before it says there is a reason to:
;;
;;   Whether the process's reading of its classpath is still the classpath.
;;   A link moved below a directory on it changes what is loaded without
;;   anything being read again - the reading names the files where they
;;   were - and a directory it was started with that is somewhere else now
;;   is one it cannot follow at all.  Both cost the process a directory per
;;   entry, so they are asked every time: the `:classpath-status' op.
;;
;;   Whether the files the classpath is made of changed.  Asked here, of a
;;   hash of each kept since the process started - see
;;   `replique-process-inputs' - so it costs no round trip, and it is the
;;   content that is compared: after a worktree is pointed somewhere else a
;;   deps.edn that says the same thing is a classpath that has not changed.
;;
;;   What they would make of the classpath now.  The deps tool's question,
;;   asked by the process with the configuration it was started with: a
;;   second or two and the network where something is not downloaded yet.
;;   The `:classpath-plan' op, whose answer says whether the difference can
;;   be added to the running process or needs a new one.
;;
;; WHAT CAN BE ADDED IS ASKED ABOUT FIRST, AND WHAT CANNOT IS A RESTART
;; OFFERED.  Adding is cheap and nearly always right, and it is still a
;; change to a running program that somebody should see happen.  A library
;; at another version cannot be added: the old one is on the loader that is
;; asked first, and whatever was loaded from it stays loaded.  What was
;; taken out of deps.edn is said and nothing else, because a loader cannot
;; forget what it was given and what is left over is usually harmless.
;;
;; AND THE NPM PACKAGES, WHICH ARE A DIFFERENT KIND OF FACT.  Nothing about
;; them is on the classpath, and the ClojureScript compiler bundles whatever
;; node_modules holds on the next compile without being asked.  What it
;; cannot know is whether node_modules holds what package.json and
;; package-lock.json say - which is the case after a checkout that changed
;; them, and the case where node_modules was installed somewhere else and
;; copied.  So that is compared, package by package, and said.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'replique-completion)
(require 'replique-parse)
(require 'replique-process)
(require 'replique-repl)

(defcustom replique-classpath-check-on-save t
  "Whether saving a file a process's classpath is made of asks about it.

The deps.edn of the project, the one of your own and
`replique-aliases-file' - and package.json, for the npm packages.  What is
asked is what \\[replique-sync-classpath] asks, and anything it would add
is asked about before it is added."
  :type 'boolean
  :group 'replique)

(defun replique-classpath--soon (function)
  "Call FUNCTION from the command loop rather than from a process filter.

What runs on the heels of an answer runs while Emacs is reading from a
socket, and asking somebody a question from there is asking it in the
middle of a read."
  (run-at-time 0 nil function))

;;; What changed on this side

(defun replique-classpath--changed (process)
  "What the classpath of PROCESS is made of that changed, or nil.

A plist of `:files', the files whose content is not what it was, and
`:launch', what the process would be started with now where that is not
what it was last seen with.  Nil where nothing changed, and where there
is nothing to compare with: a process with no directory, or one this
Emacs knows nothing about the start of."
  (when-let* ((inputs (replique-process--inputs process))
              (directory (replique-process--directory process)))
    (let* ((now (replique-process-inputs directory))
           (files (seq-keep (lambda (entry)
                              (unless (equal (cdr entry)
                                             (cdr (assoc (car entry)
                                                         (plist-get inputs :files))))
                                (car entry)))
                            (plist-get now :files)))
           (seen (or (plist-get inputs :seen) (plist-get inputs :launch)))
           (launch (unless (equal seen (plist-get now :launch))
                     (plist-get now :launch))))
      (when (or files launch)
        (list :files files :launch launch)))))

(defun replique-classpath--configuration (process)
  "What to tell PROCESS about how it would be started now.

Nothing where that is how it was started, which is not the same as
saying it: a process this Emacs only connected to was started with
whatever its owner typed, and the one thing known about that is that it
is what it was when this Emacs first saw it."
  (when-let* ((inputs (replique-process--inputs process))
              (directory (replique-process--directory process)))
    (let ((launch (replique-process-launch directory)))
      (unless (equal launch (plist-get inputs :launch))
        (list :aliases (vconcat (plist-get launch :aliases))
              :extra (plist-get launch :extra))))))

(defun replique-classpath--seen (process)
  "Remember that the classpath of PROCESS was looked at as it is now.

The files, so that what has been said once is not asked again until one
of them changes again.  Not the launch it was started with, which goes on
being what the process is told it is compared against - see
`replique-classpath--configuration'."
  (when-let* ((inputs (replique-process--inputs process))
              (directory (replique-process--directory process)))
    (let ((now (replique-process-inputs directory)))
      (setf (replique-process--inputs process)
            (list :launch (plist-get inputs :launch)
                  :seen (plist-get now :launch)
                  :files (plist-get now :files))))))

;;; The npm packages

(defun replique-classpath--npm-root (directory)
  "The nearest directory at or above DIRECTORY that has a node_modules.

Node's own rule, and the one the ClojureScript compiler finds its
packages by - see clojure.cljs.npm/project-root."
  (when-let* ((root (locate-dominating-file
                     directory
                     (lambda (dir)
                       (file-directory-p (expand-file-name "node_modules" dir))))))
    (file-name-as-directory (expand-file-name root))))

(defun replique-classpath--json (file)
  "What FILE holds, read as JSON, or nil where it cannot be read."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-buffer :object-type 'hash-table :null-object nil
                             :false-object nil))
      (error nil))))

(defun replique-classpath--registry-p (spec)
  "Whether the package.json SPEC names a version of the npm registry.

As against a directory, a link, a repository or a tarball, whose version
is whatever is there - so what can be asked of one is only whether it is
installed at all."
  (not (string-match-p
        "\\`\\(?:file:\\|link:\\|workspace:\\|git\\|github:\\|https?:\\|[^@/][^/]*/\\)"
        spec)))

(defun replique-classpath-npm-problems (directory)
  "How node_modules differs from what the project above DIRECTORY declares.

A list of sentences, one per package, nil where nothing does.  The
packages are what package.json declares, its dependencies and its
devDependencies, which is what an install installs.  Each is looked for
under node_modules, and its version there is held up against the one
package-lock.json has for it - the exact version an install writes, where
package.json has only a range.

THE CONTENT AND NOT THE DATES.  node_modules is not always installed where
it is read: it is copied from another checkout, or reached through a link,
and the times on its files are those of the copy.  What is installed is
what each package says its version is."
  (when-let* ((root (replique-classpath--npm-root directory))
              (package (replique-classpath--json (expand-file-name "package.json" root))))
    (let* ((lock (replique-classpath--json (expand-file-name "package-lock.json" root)))
           (locked (and lock (gethash "packages" lock)))
           (problems nil))
      (dolist (key '("dependencies" "devDependencies"))
        (when-let* ((declared (gethash key package)))
          (maphash
           (lambda (name spec)
             (let* ((installed (replique-classpath--json
                                (expand-file-name (concat "node_modules/" name "/package.json")
                                                  root)))
                    (version (and installed (gethash "version" installed)))
                    (entry (and locked (gethash (concat "node_modules/" name) locked)))
                    (wanted (and entry (gethash "version" entry))))
               (cond
                ((null installed)
                 (push (format "%s is not installed" name) problems))
                ((not (and (stringp spec) (replique-classpath--registry-p spec))))
                ((and locked (null entry))
                 (push (format "%s is in package.json and not in package-lock.json" name)
                       problems))
                ((and wanted version (not (equal wanted version)))
                 (push (format "%s %s is installed, package-lock.json has %s"
                               name version wanted)
                       problems)))))
           declared)))
      (sort problems #'string<))))

;;; Asking the process

(defun replique-classpath--answered-p (frame)
  "Whether FRAME is an answer, rather than a refusal."
  (and frame (not (equal "error" (plist-get frame :tag)))))

(defun replique-classpath-check (process callback &optional plan act)
  "Find out whether the classpath of PROCESS is still the right one.

CALLBACK is called with a plist, from a process filter:

  :reading-due  the reading of the classpath had moved
  :rescanned    and it was read again
  :frozen       the directories the process cannot follow any more
  :plan         the `:classpath-plan' answer, where one was asked for
  :changed      what changed on this side, as `replique-classpath--changed'
  :npm          what `replique-classpath-npm-problems' found

PLAN asks for a plan whether or not anything changed on this side.

ACT has a reading that is due done before the plan is asked for: the
reading is what the plan compares against, and it is what completion
and the staleness questions after it read.  Without ACT nothing in the
process changes - what is due is only said.

A process too old to answer one of the ops is a process that says
nothing about it."
  (let* ((changed (replique-classpath--changed process))
         (directory (replique-process--directory process))
         (report (list :changed changed
                       :npm (and directory (replique-classpath-npm-problems directory))))
         (finish
          (lambda ()
            (if (not (or plan changed))
                (funcall callback report)
              (message "replique: resolving the classpath...")
              (replique-process-request
               process
               (append (list :op :classpath-plan)
                       (replique-classpath--configuration process))
               (lambda (frame)
                 (setq report (plist-put report :plan frame))
                 (funcall callback report)))))))
    (replique-process-request
     process (list :op :classpath-status)
     (lambda (status)
       (when (replique-classpath--answered-p status)
         (setq report (plist-put report :frozen (plist-get status :frozen)))
         (setq report (plist-put report :reading-due (plist-get status :reading-due))))
       (if (and act (plist-get report :reading-due))
           (replique-process-request
            process (list :op :update-classpath)
            (lambda (_frame)
              (setq replique-completion--last nil)
              (setq report (plist-put report :rescanned t))
              (funcall finish)))
         (funcall finish))))))

;;; What it comes to

(defun replique-classpath--plan (report)
  "The plan REPORT carries, where it is one the process answered."
  (let ((plan (plist-get report :plan)))
    (when (replique-classpath--answered-p plan) plan)))

(defun replique-classpath--restart-reasons (report)
  "Why REPORT is a process to restart, as phrases, or nil when it is not."
  (let ((plan (replique-classpath--plan report)))
    (append
     (mapcar (lambda (frozen)
               (format "%s is read where it led when the process started"
                       (abbreviate-file-name (plist-get frozen :entry))))
             (or (plist-get report :frozen) (plist-get plan :frozen)))
     (mapcar (lambda (moved)
               (format "%s %s instead of %s"
                       (plist-get moved :lib) (plist-get moved :now)
                       (plist-get moved :was)))
             (plist-get plan :moved))
     (mapcar (lambda (shadowed)
               (format "%s would be behind what provides %s"
                       (abbreviate-file-name (plist-get shadowed :entry))
                       (string-join (plist-get shadowed :namespaces) ", ")))
             (plist-get plan :shadowed))
     (when (plist-get plan :jvm-opts)
       (list "other jvm options")))))

(defun replique-classpath--additions (report)
  "What REPORT says can be added to the running process, as phrases."
  (let ((plan (replique-classpath--plan report)))
    (when (equal "additive" (plist-get plan :verdict))
      (append (mapcar (lambda (lib)
                        (format "%s %s" (plist-get lib :lib) (plist-get lib :now)))
                      (plist-get plan :added-libs))
              (mapcar #'abbreviate-file-name (plist-get plan :added-paths))))))

(defun replique-classpath--removals (report)
  "What REPORT says is no longer declared and is still loaded, as phrases."
  (let ((plan (replique-classpath--plan report)))
    (append (plist-get plan :removed-libs)
            (mapcar #'abbreviate-file-name (plist-get plan :removed-paths)))))

(defun replique-classpath--listed (phrases)
  "PHRASES as a list in a sentence, the first few of them."
  (if (> (length phrases) 3)
      (format "%s and %d more" (string-join (seq-take phrases 3) ", ")
              (- (length phrases) 3))
    (string-join phrases ", ")))

(defun replique-classpath--npm-sentence (report)
  "What REPORT says about the npm packages, in a sentence, or nil."
  (when-let* ((problems (plist-get report :npm)))
    (display-warning
     'replique
     (format "node_modules is not what this project declares:\n\n  %s\n\n%s"
             (string-join problems "\n  ")
             (concat "The ClojureScript compiler bundles what node_modules holds."
                     "  Install the packages where node_modules comes from.")))
    (format "node_modules differs from package.json for %d package%s - see *Warnings*"
            (length problems) (if (cdr problems) "s" ""))))

;;; Doing something about it

(defun replique-classpath--sync (process done)
  "Add to PROCESS what its plan adds, then call DONE with a sentence."
  (replique-process-request
   process
   (append (list :op :sync-classpath) (replique-classpath--configuration process))
   (lambda (frame)
     (setq replique-completion--last nil)
     (funcall done
              (if (replique-classpath--answered-p frame)
                  (progn
                    (replique-classpath--seen process)
                    (format "added %s to the classpath"
                            (replique-classpath--listed (plist-get frame :added))))
                (format "the classpath could not be added to: %s"
                        (plist-get frame :message)))))))

(defun replique-classpath-act (process report then)
  "Do what REPORT says the classpath of PROCESS needs, asking first.

THEN is called with a sentence saying what was found and done, or nil
where there is nothing to say, and with whether to go on - which is nil
only where the process is being restarted, and anything that was going to
be done to it next would be done to a process on its way out.

A RESTART IS OFFERED, and declining it is said and changes nothing: it
is asked again the next time, since nothing has made it any less true.
WHAT CAN BE ADDED IS ASKED ABOUT, and added when the answer is yes.  WHAT
WAS TAKEN OUT is said, once."
  (replique-classpath--soon
   (lambda ()
     (let* ((id (replique-process--id process))
            (restart (replique-classpath--restart-reasons report))
            (additions (replique-classpath--additions report))
            (removals (replique-classpath--removals report))
            (npm (replique-classpath--npm-sentence report))
            (said (lambda (&rest parts)
                    (let ((parts (delq nil (append parts (list npm)))))
                      (when parts (string-join parts " - "))))))
       (cond
        (restart
         (if (y-or-n-p (format "%s cannot follow its classpath: %s.  Restart it? "
                               id (replique-classpath--listed restart)))
             (condition-case err
                 (progn
                   (replique-restart process)
                   (funcall then (funcall said (format "restarting %s" id)) nil))
               ;; A process this Emacs did not start and cannot stop
               (user-error
                (funcall then (funcall said (error-message-string err)) t)))
           (funcall then
                    (funcall said
                             (format "%s cannot follow its classpath (%s) - M-x replique-restart"
                                     id (replique-classpath--listed restart)))
                    t)))
        (additions
         (if (y-or-n-p (format "Add %s to %s? "
                               (replique-classpath--listed additions) id))
             (replique-classpath--sync
              process (lambda (sentence) (funcall then (funcall said sentence) t)))
           (funcall then
                    (funcall said
                             (format "%s is not on the classpath yet - M-x replique-sync-classpath"
                                     (replique-classpath--listed additions)))
                    t)))
        (t
         (when (replique-classpath--plan report)
           (replique-classpath--seen process))
         (funcall then
                  (funcall said
                           (when removals
                             (format "%s left deps.edn and stays loaded until a restart"
                                     (replique-classpath--listed removals)))
                           (replique-classpath--unplanned report))
                  t)))))))

(defun replique-classpath--unplanned (report)
  "Why the plan REPORT asked for could not be made, in a sentence, or nil.

Nil where the process is one that cannot be asked - too old to know the
op, or not started by the clojure cli, which is what the plan is made
against - since that is not news and would be said on every reload.  The
rest is the deps tool refusing the deps files, which is exactly what
somebody who just edited one needs to hear."
  (let ((plan (plist-get report :plan)))
    (when (and plan
               (not (replique-classpath--answered-p plan))
               (not (member (plist-get plan :error) '("unknown-op" "no-basis"))))
      (format "the deps files could not be resolved: %s"
              (string-trim (or (plist-get plan :message) ""))))))

;;;###autoload
(defun replique-sync-classpath (process)
  "Bring the classpath of PROCESS up to date with its deps files.

As far as a running process can be brought: what is new is added, after
asking, and what cannot be added - another version of a library it has,
a directory it was started with that is somewhere else now - is a restart
it offers instead.  The deps tool is asked whatever changed, which takes
a second or two.

\\[replique-reload-app] asks the same, but only where one of the files
the classpath is made of changed since the process started."
  (interactive (list (replique-process-ensure)))
  (replique-classpath-check
   process
   (lambda (report)
     (replique-classpath-act
      process report
      (lambda (sentence _go-on)
        (message "replique: %s"
                 (or sentence
                     (let ((plan (plist-get report :plan)))
                       (if (replique-classpath--answered-p plan)
                           "the classpath is what the deps files say"
                         (format "the classpath could not be planned: %s"
                                 (plist-get plan :message)))))))))
   t t))

;;; On save

(defun replique-classpath--watching (file)
  "The live processes whose classpath FILE is one of the inputs of."
  (let ((true (file-truename file)))
    (seq-filter
     (lambda (process)
       (seq-some (lambda (entry) (equal true (file-truename (car entry))))
                 (plist-get (replique-process--inputs process) :files)))
     (replique-processes-live))))

(defun replique-classpath--npm-watching (file)
  "The live processes whose npm root FILE is the package.json of."
  (when (equal "package.json" (file-name-nondirectory file))
    (let ((true (file-truename file)))
      (seq-filter
       (lambda (process)
         (when-let* ((directory (replique-process--directory process))
                     (root (replique-classpath--npm-root directory)))
           (equal true (file-truename (expand-file-name "package.json" root)))))
       (replique-processes-live)))))

(defun replique-classpath--after-save ()
  "Ask about the classpath of the processes the file just saved is made of."
  (when (and replique-classpath-check-on-save buffer-file-name)
    (dolist (process (replique-classpath--watching buffer-file-name))
      (replique-classpath-check
       process
       (lambda (report)
         (replique-classpath-act
          process report
          (lambda (sentence _go-on)
            (when sentence (message "replique: %s" sentence)))))))
    (dolist (process (replique-classpath--npm-watching buffer-file-name))
      (when-let* ((sentence (replique-classpath--npm-sentence
                             (list :npm (replique-classpath-npm-problems
                                         (replique-process--directory process))))))
        (message "replique: %s" sentence)))))

(add-hook 'after-save-hook #'replique-classpath--after-save)

;;; The directories a deps.edn declares

;; A project's own classpath directories, read off its deps.edn without the
;; deps tool: :paths, and the :extra-paths and :replace-paths of the aliases it
;; is started with.  No libraries and nothing resolved - what a tool laying the
;; project out on disk has to know before any process runs.

(defun replique-classpath--entries (node)
  "The entries of the map NODE, as (KEY-TEXT . VALUE-NODE).
Nil for anything that is not a map, so a deps.edn in a shape this does
not expect answers nothing rather than signalling."
  (when (and node (eq 'map (replique-parse-type node)))
    (let (found)
      (dolist (child (replique-parse-forms node))
        (when (eq 'pair (replique-parse-type child))
          (let ((forms (replique-parse-forms child)))
            (push (cons (replique-parse-text (car forms)) (cadr forms)) found))))
      (nreverse found))))

(defun replique-classpath--strings (node)
  "The strings written in the vector or list NODE, without their quotes.
Substring rather than a reader: right for the directory names a :paths
holds, wrong for a string with an escape in it."
  (when (and node (memq (replique-parse-type node) '(vector list)))
    (seq-keep (lambda (child)
                (when (eq 'string (replique-parse-type child))
                  (let ((text (replique-parse-text child)))
                    (substring text 1 (1- (length text))))))
              (replique-parse-forms node))))

(defun replique-classpath-directories (directory &optional aliases)
  "The classpath directories the deps.edn of DIRECTORY declares.
Its :paths, and the :extra-paths and :replace-paths of each of ALIASES,
relative to DIRECTORY as written.  ALIASES defaults to the ones a process
started in DIRECTORY is started with - see
`replique-process--project-aliases'."
  (let ((file (expand-file-name "deps.edn" directory)))
    (when (file-readable-p file)
      (let ((aliases (mapcar (lambda (alias) (string-remove-prefix ":" alias))
                             (or aliases
                                 (replique-process--project-aliases directory)))))
        (with-temp-buffer
          (insert-file-contents file)
          (let* ((entries (replique-classpath--entries
                           (car (replique-parse-forms (replique-parse-buffer)))))
                 (paths (replique-classpath--strings
                         (cdr (assoc ":paths" entries)))))
            (dolist (alias (replique-classpath--entries
                            (cdr (assoc ":aliases" entries))))
              (when (member (string-remove-prefix ":" (car alias)) aliases)
                (dolist (key '(":extra-paths" ":replace-paths"))
                  (setq paths
                        (append paths
                                (replique-classpath--strings
                                 (cdr (assoc key (replique-classpath--entries
                                                  (cdr alias))))))))))
            (delete-dups paths)))))))

(provide 'replique-classpath)

;;; replique-classpath.el ends here
