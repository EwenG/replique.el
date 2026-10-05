;;; replique-cli.el --- The process of a project, from a shell  -*- lexical-binding: t; -*-

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

;; What bin/rq runs, in a batch Emacs: a command line for the replique
;; process that reads the project it is run from, for an agent that has a
;; shell and no editor.  It speaks the protocol through replique.el's own
;; connection code rather than a second client of it, so the two cannot drift.
;;
;; WHICH PROCESS.  One running in the project itself, when there is one, and
;; otherwise one in a directory `replique-cli-process-directory-functions'
;; names - a directory of links into the project, say, that a process runs
;; in and is pointed at one checkout or another.  Nothing else: a process
;; reading another checkout would answer about another branch, and loading
;; this one's files into it would mix the two.  With neither, every command
;; refuses with exit status 3, which is the agent's cue to ask the user.
;;
;; Configuration that is yours rather than the project's - those functions,
;; and what to tell an agent - goes in a file loaded after this one: the one
;; $REPLIQUE_CLI_INIT names, or one a wrapper of bin/rq passes with -l.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'replique-classpath)
(require 'replique-conn)
(require 'replique-css)
(require 'replique-edn)
(require 'replique-exception)
(require 'replique-process)
(require 'replique-format)
(require 'replique-main-js)

(defvar replique-cli-process-directory-functions nil
  "Functions naming another directory whose process reads the project.
Each is called with the project\='s directory, as a truename, and answers
a directory or nil.  The first answer is the one asked, and only where no
process runs in the project itself.")

(defvar replique-cli-no-process-function nil
  "Function saying what to do when no process reads the project, or nil.
Called with the project\='s directory, and answers the lines a refusal
ends with - which is what an agent hands on to the user.")

(defvar replique-cli-no-page-hint "Open this process's page."
  "What to do when no page is connected to the ClojureScript runtime.")

(define-error 'replique-cli-stop "rq")

(defun replique-cli-stop (code format-string &rest args)
  "Stop with exit status CODE, saying FORMAT-STRING with ARGS on stderr."
  (signal 'replique-cli-stop (list code (apply #'format format-string args))))

(defun replique-cli-out (format-string &rest args)
  "Print FORMAT-STRING with ARGS, and a newline, on stdout."
  (princ (concat (apply #'format format-string args) "\n")))

(defun replique-cli-err (string)
  "Write STRING on stderr as it is."
  (princ string #'external-debugging-output))

;;; Which process

(defun replique-cli--project ()
  "The project the command was run from, as a truename, or nil.
The nearest deps.edn above it, which is what a process runs in."
  (when-let* ((root (locate-dominating-file
                     default-directory
                     (lambda (dir) (file-exists-p (expand-file-name "deps.edn" dir))))))
    (directory-file-name (file-truename root))))

(defun replique-cli--live (directory)
  "What the process running in DIRECTORY says about itself, or nil.
Only a process that answers: a port file a crash left behind names nothing."
  (seq-some (lambda (description)
              (let ((info (cdr description)))
                (and (plist-get info :port)
                     (replique-process--listening-p (plist-get info :host)
                                                    (plist-get info :port))
                     info)))
            (replique-process-descriptions directory)))

(defun replique-cli--elsewhere (project)
  "The directory another process reading PROJECT would run in, or nil."
  (and project
       (run-hook-with-args-until-success
        'replique-cli-process-directory-functions project)))

(defun replique-cli--process ()
  "The description of the process that reads this project, or a refusal."
  (let* ((project (replique-cli--project))
         (elsewhere (replique-cli--elsewhere project)))
    (or (and project (replique-cli--live project))
        (and elsewhere
             (or (replique-cli--live elsewhere)
                 (replique-cli-stop 3 "rq: %s reads this project, but no process runs there.
    Ask the user to start one (M-x replique-connect from a buffer there)."
                                    elsewhere)))
        (replique-cli-stop 3 "rq: no replique process reads %s.
%s"
                           (or project default-directory)
                           (or (and replique-cli-no-process-function
                                    (funcall replique-cli-no-process-function project))
                               "    Ask the user to start one (M-x replique-connect from one of its files).")))))

(defun replique-cli--rel (path)
  "PATH as the file it is, relative to the project when it is in it.
The process names files by the path it read them through, which for a
directory of links is a link into the project."
  (if (not (and path (file-exists-p path)))
      path
    (let ((true (file-truename path))
          (project (replique-cli--project)))
      (if (and project (string-prefix-p (file-name-as-directory project) true))
          (file-relative-name true project)
        true))))

;;; Connections

(defun replique-cli--connect (info kind &optional hello on-frame)
  "A connection of KIND to the process INFO describes, handshake done.
HELLO holds the handshake's own keys, ON-FRAME receives every frame."
  (let* ((state nil)
         (refusal nil)
         (conn (replique-conn-open (plist-get info :host) (plist-get info :port) kind
                                   :process-id (plist-get info :process-id)
                                   :hello hello
                                   :on-frame on-frame
                                   :on-ready (lambda (_) (setq state 'ready))
                                   :on-error (lambda (frame)
                                               (setq state 'refused refusal frame)))))
    (while (null state)
      (accept-process-output (replique-conn--proc conn) 0.05))
    (when (eq state 'refused)
      (replique-cli-stop 2 "rq: the handshake was refused: %s (%s)"
               (plist-get refusal :message) (plist-get refusal :error)))
    conn))

(defun replique-cli--control (info msg &optional timeout)
  "Ask MSG of the process INFO describes and return the reply.
An error frame is signalled as `replique-cli-refused', with the frame."
  (let ((conn (replique-cli--connect info 'control)))
    (unwind-protect
        (let ((reply (replique-conn-request-sync conn msg (or timeout 60))))
          (if (equal "error" (plist-get reply :tag))
              (signal 'replique-cli-refused (list reply))
            reply))
      (replique-conn-close conn))))

(define-error 'replique-cli-refused "The process refused the request")

(defun replique-cli--refusal-text (frame)
  "What the error FRAME says."
  (format "%s: %s" (plist-get frame :error)
          (or (plist-get frame :message)
              (plist-get (plist-get frame :exception) :message))))

(defconst replique-cli--done ":rq/done"
  "The value the form that ends every evaluation returns.
A repl answers each form with one prompt, so the end of what was sent is
the prompt after a form the client knows the value of.")

(defun replique-cli--print-exception (frame)
  "Print the exception FRAME on stderr.
A failure to read or compile is its message and its position: the
compiler's own frames under it say nothing about the code."
  (replique-cli-err (format "!! %s%s\n"
                  (if-let* ((phase (plist-get frame :phase))) (concat phase ": ") "")
                  (plist-get frame :message)))
  (unless (string-match-p "\\`\\(read\\|macro\\|compile\\)" (or (plist-get frame :phase) ""))
    (let ((first t))
      (dolist (cause (replique-exception-chain (plist-get frame :exception)))
        (replique-cli-err (format "   %s%s: %s\n" (if first "" "caused by ")
                        (plist-get cause :class) (plist-get cause :message)))
        (when-let* ((data (plist-get cause :data)))
          (replique-cli-err (format "   data: %s\n" data)))
        (dolist (line (seq-take (replique-exception--fold (plist-get cause :trace)) 8))
          (replique-cli-err (format "     at %s\n" line)))
        (setq first nil))))
  (when-let* ((trace (plist-get frame :stacktrace)))
    (replique-cli-err (concat (string-join (seq-take (split-string trace "\n") 20) "\n") "\n"))))

(cl-defun replique-cli--repl (info code &key cljs ns setup quiet on-out (timeout 120))
  "Send CODE on a new repl of the process INFO describes.
Non-nil when nothing threw.  CLJS opens a ClojureScript repl on the
browser.  NS is the namespace CODE is read in, SETUP a form sent first
whose value is not shown, QUIET leaves the values out and keeps only
output and failures.  ON-OUT, when given, is handed the output instead
of it being printed."
  ;; Asked first because a form sent to no page is answered with an
  ;; exception rather than a value - the form that ends the evaluation too, so
  ;; the end would never be seen
  (when (and cljs (not (plist-get (replique-cli--control info '(:op :stale :dialect :cljs))
                                  :connected)))
    (replique-cli-stop 3 "rq: no page is connected to the ClojureScript runtime.
    %s" replique-cli-no-page-hint))
  (let* ((skip (if setup 1 0))
         (done nil)
         (ok t)
         (on-frame
          (lambda (frame)
            (pcase (plist-get frame :tag)
              ("out" (funcall (or on-out #'princ) (plist-get frame :string)))
              ("err" (replique-cli-err (plist-get frame :string)))
              ("event" (when (equal "error" (plist-get frame :event))
                         (replique-cli-err (format "rq: %s\n" (plist-get frame :message)))
                         (setq ok nil done t)))
              ("ret" (cond ((> skip 0) (cl-decf skip))
                           ((equal replique-cli--done (plist-get frame :value)) (setq done t))
                           ((not quiet) (replique-cli-out "=> %s" (plist-get frame :value)))))
              ("exception" (replique-cli--print-exception frame)
                           (setq ok nil)
                           (when (> skip 0) (cl-decf skip))))))
         (conn (replique-cli--connect info 'repl
                            (when cljs '(:dialect :cljs :target :browser))
                            on-frame))
         (deadline (+ (float-time) timeout)))
    (setf (replique-conn--on-close conn) (lambda (_) (setq done t ok nil)))
    (unwind-protect
        (progn
          (replique-conn-send-code
           conn (concat (when setup (concat setup "\n"))
                        (when ns (format "#replique/ns %s\n" ns))
                        code "\n" replique-cli--done))
          (while (not done)
            (accept-process-output (replique-conn--proc conn) 0.1)
            (when (> (float-time) deadline)
              (ignore-errors
                (replique-cli--control info (list :op :interrupt :connection (replique-conn--id conn))))
              (replique-cli-stop 2 "rq: no answer in %ss - interrupted it. Pass --timeout for longer."
                       timeout)))
          ok)
      (replique-conn-close conn))))

;;; Commands

(defun replique-cli--options (args)
  "Split ARGS into a plist of options and the list of the rest."
  (let ((opts (list :timeout 120)) (rest nil))
    (while args
      (let ((a (pop args)))
        (pcase a
          ("--cljs" (setq opts (plist-put opts :cljs t)))
          ("--clj" (setq opts (plist-put opts :clj t)))
          ("--full" (setq opts (plist-put opts :full t)))
          ("--check" (setq opts (plist-put opts :check t)))
          ("--ns" (setq opts (plist-put opts :ns (pop args))))
          ("--main" (setq opts (plist-put opts :main (pop args))))
          ("--file" (setq opts (plist-put opts :file (pop args))))
          ("--timeout" (setq opts (plist-put opts :timeout (string-to-number (pop args)))))
          (_ (push a rest)))))
    (list opts (nreverse rest))))

(defun replique-cli--load-form (file)
  "The directive that loads FILE."
  (format "#replique/load {:file %s}" (replique-edn-string (expand-file-name file))))

(defun replique-cli--dialects (file)
  "The compilers that read FILE."
  (cond ((string-suffix-p ".cljc" file) '(clj cljs))
        ((string-suffix-p ".cljs" file) '(cljs))
        (t '(clj))))

(defun replique-cli-info (_args)
  "Which process reads this project."
  (let* ((project (replique-cli--project))
         (elsewhere (replique-cli--elsewhere project)))
    (replique-cli-out "project    %s" (or project "(no deps.edn above here)"))
    (when elsewhere
      (replique-cli-out "elsewhere  %s" elsewhere))
    (let* ((info (replique-cli--process))
           (reply (replique-cli--control info '(:op :process-info))))
      (replique-cli-out "process    %s in %s, pid %s, port %s, up %s min"
              (plist-get info :process-id) (plist-get info :directory)
              (plist-get info :pid) (plist-get info :port)
              (/ (or (plist-get reply :uptime) 0) 60000))
      (replique-cli-out "analysis   %s, cljs %s"
              (if (plist-get reply :analysis) "yes" "no")
              (if (plist-get reply :cljs) "yes" "no"))
      t)))

(defun replique-cli--say-stale (dialect reply)
  "Print what the :stale REPLY for DIALECT says."
  (replique-cli-out "%s: %d changed, %d stale%s, %s analysed%s%s"
          dialect (length (plist-get reply :changed)) (length (plist-get reply :stale))
          (if (plist-get reply :deleted)
              (format ", %d deleted" (length (plist-get reply :deleted))) "")
          (plist-get reply :analysed)
          (if (> (or (plist-get reply :unread) 0) 0)
              (format ", %s running but never analysed" (plist-get reply :unread)) "")
          (cond ((not (plist-member reply :connected)) "")
                ((plist-get reply :connected) ", page connected")
                (t ", no page connected")))
  (dolist (key '(:changed :stale :deleted))
    (dolist (f (plist-get reply key))
      (replique-cli-out "  %-8s%s" (substring (symbol-name key) 1) (replique-cli--rel (plist-get f :file))))))

(defun replique-cli-stale (_args)
  "What changed on disk since the process read it."
  (let ((info (replique-cli--process)))
    (replique-cli--say-stale "clj" (replique-cli--control info '(:op :stale)))
    (replique-cli--say-stale "cljs" (replique-cli--control info '(:op :stale :dialect :cljs)))
    t))

(defun replique-cli-eval (args)
  "Evaluate code."
  (pcase-let* ((`(,opts ,rest) (replique-cli--options args))
               (info (replique-cli--process))
               (code (cond ((plist-get opts :file)
                            (with-temp-buffer
                              (insert-file-contents (plist-get opts :file))
                              (buffer-string)))
                           ((or (null rest) (equal rest '("-")))
                            (let ((lines nil) line)
                              (while (setq line (ignore-errors (read-from-minibuffer "")))
                                (push line lines))
                              (string-join (nreverse lines) "\n")))
                           (t (string-join rest " ")))))
    ;; replique's repl prints fifteen items deep, and an agent reading a
    ;; truncated map takes the `...' for the data
    (replique-cli--repl info code
              :cljs (plist-get opts :cljs)
              :ns (plist-get opts :ns)
              :timeout (plist-get opts :timeout)
              :setup (unless (plist-get opts :cljs)
                       (if (plist-get opts :full)
                           "(do (set! *print-length* nil) (set! *print-level* nil))"
                         "(do (set! *print-length* 200) (set! *print-level* 12))")))))

(defun replique-cli-load (args)
  "Load files as units."
  (pcase-let ((`(,opts ,files) (replique-cli--options args)))
    (unless files (replique-cli-stop 2 "rq: load what? rq load FILE..."))
    (dolist (f files)
      (unless (file-exists-p f) (replique-cli-stop 2 "rq: no file %s" f)))
    (let ((ok (replique-cli--repl (replique-cli--process) (mapconcat #'replique-cli--load-form files "\n")
                        :quiet t
                        :cljs (or (plist-get opts :cljs)
                                  (seq-every-p (lambda (f) (string-suffix-p ".cljs" f)) files))
                        :timeout (plist-get opts :timeout))))
      (when ok (replique-cli-out "loaded %s" (string-join files ", ")))
      ok)))

(defun replique-cli-reload (args)
  "Load everything that changed, the Clojure program then the ClojureScript one.
A reload reloads the program of the repl it is sent on, so the two are two
reloads, the second only when it has work: opening that repl costs a
compile environment."
  (let* ((opts (car (replique-cli--options args)))
         (timeout (max 300 (plist-get opts :timeout)))
         (info (replique-cli--process))
         (clj (progn (replique-cli-out "clj:") (replique-cli--repl info "#replique/reload {}" :timeout timeout)))
         (stale (replique-cli--control info '(:op :stale :dialect :cljs)))
         (cljs (or (and (null (plist-get stale :changed)) (null (plist-get stale :stale)))
                   (progn (replique-cli-out "cljs:")
                          (replique-cli--repl info "#replique/reload {}" :cljs t :timeout timeout))))
         ;; Built whatever the languages did, as `replique-reload-app' does:
         ;; sass has no opinion about a macro.  A page that isn't open is not
         ;; a failure here - the build is on the disk for the next one
         (css (progn (replique-cli-out "css:")
                     (not (eq :failed (replique-cli--css info))))))
    (and clj cljs css)))

(defun replique-cli--css (info)
  "Build the stylesheets of the process INFO and reload them in its pages.
Say what happened, and return :reloaded, :no-page, :failed, or nil where
the project says nothing about stylesheets.

What to build and what the build writes are `replique-css-entry' and
`replique-css-outputs', which the project sets in the `.dir-locals.el' of
the process's directory - the same two the editor's key reads."
  (let ((root (file-name-as-directory (plist-get info :directory)))
        (project-dir default-directory)
        ;; None of the three is marked safe, and a batch Emacs can't ask
        (enable-local-variables :all))
    (with-temp-buffer
      (setq default-directory root)
      (hack-dir-local-variables-non-file-buffer)
      (cond
       ((not (replique-css-configured-p))
        (replique-cli-out "nothing to build: no replique-css-entry / replique-css-outputs in %s.dir-locals.el"
                root)
        nil)
       ((when-let* ((failed (replique-css-build root)))
          ;; Sass prints a deprecation warning per @import ahead of the
          ;; error, hundreds of lines of them on this project
          (replique-cli-err (concat (if (string-match "^Error: " failed)
                              (substring failed (match-beginning 0))
                            failed)
                          "\n"))
          (replique-cli-out "the stylesheet build failed")
          t)
        :failed)
       (t
        (let ((outputs (replique-css-outputs-in root)))
          (replique-cli-out "built %s" (let ((default-directory project-dir))
                                (string-join (mapcar #'replique-cli--rel outputs) ", ")))
          (if (not (plist-get (replique-cli--control info '(:op :stale :dialect :cljs)) :connected))
              (progn (replique-cli-out "no page is connected to reload it in")
                     :no-page)
            (let ((frames (mapcar (lambda (output)
                                    (replique-cli--control info (list :op :reload-css :file output)))
                                  outputs)))
              (replique-cli-out "%s" (replique-css--sentence outputs frames))
              (if (seq-some (lambda (f) (plist-get f :reloaded)) frames)
                  :reloaded
                :failed)))))))))

(defun replique-cli-css (_args)
  "Build the stylesheets and reload them in the page, without reloading it."
  (pcase (replique-cli--css (replique-cli--process))
    ('nil (replique-cli-stop 2 "rq: this project says nothing about its stylesheets"))
    (:no-page (replique-cli-stop 3 "rq: no page is connected to the ClojureScript runtime.
    %s" replique-cli-no-page-hint))
    (:reloaded t)
    (:failed nil)))

(defun replique-cli--source-directories ()
  "The project\='s classpath directories, as its deps.edn declares them.
Under the aliases its .dir-locals.el starts a process with, read without
asking: a batch Emacs can\='t."
  (let ((project (or (replique-cli--project) default-directory))
        (enable-local-variables :all))
    (mapcar (lambda (dir) (expand-file-name dir project))
            (or (replique-classpath-directories project) '("src" "test")))))

(defun replique-cli--ns-file (ns)
  "The file NS is in, under one of the project\='s classpath directories, or nil."
  (let ((base (replace-regexp-in-string
               "-" "_" (replace-regexp-in-string "\\." "/" ns))))
    (seq-some (lambda (candidate)
                (and (file-exists-p candidate) (file-relative-name candidate)))
              (cl-loop for dir in (replique-cli--source-directories)
                       append (cl-loop for ext in '(".clj" ".cljc" ".cljs")
                                       collect (expand-file-name (concat base ext) dir))))))

(defun replique-cli--test-clj (info tests timeout)
  "Run TESTS, (NAME NS VAR FILE) lists, in the JVM."
  (replique-cli--repl
   info
   (concat
    "#replique/reload {}\n"
    (mapconcat #'replique-cli--load-form (delete-dups (mapcar #'cl-fourth tests)) "\n") "\n"
    "(require 'clojure.test)\n"
    ;; One form, so that the verdict is its value or its exception
    "(let [t (binding [clojure.test/*test-out* *out*] (->> ["
    (mapconcat (lambda (test)
                 (if (nth 2 test)
                     (format "(clojure.test/run-test-var #'%s)" (car test))
                   (format "(clojure.test/run-tests '%s)" (nth 1 test))))
               tests " ")
    "] (map #(select-keys % [:test :pass :fail :error])) (apply merge-with +)))]"
    " (if (pos? (+ (:fail t 0) (:error t 0))) (throw (ex-info \"tests failed\" t)) t))")
   :timeout timeout))

(defconst replique-cli--tests-done ":rq/tests-done"
  "What the page prints once the last ClojureScript test has run.")

(defun replique-cli--test-cljs (info tests timeout)
  "Run TESTS, (NAME NS VAR FILE) lists, in the page.
cljs.test returns before an async test has finished, and what the page
prints after the evaluation came back is no longer the repl's: it is an
`out' event on the control connections.  So one is held open, and the
verdict is read off what was printed - cljs.test gives no value to read -
once the block that runs last has said so."
  (let* ((printed "")
         (finished nil)
         (seen (lambda (string)
                 (setq printed (concat printed string))
                 (if (string-search replique-cli--tests-done printed)
                     (setq finished t)
                   (princ string))))
         (control (replique-cli--connect info 'control nil
                               (lambda (frame)
                                 (when (and (equal "event" (plist-get frame :tag))
                                            (equal "out" (plist-get frame :event))
                                            (equal "cljs" (plist-get frame :dialect)))
                                   (funcall seen (plist-get frame :string))))))
         (deadline (+ (float-time) timeout)))
    (unwind-protect
        (let ((ran (replique-cli--repl
                    info
                    (concat
                     "#replique/reload {}\n"
                     (mapconcat #'replique-cli--load-form (delete-dups (mapcar #'cl-fourth tests)) "\n")
                     "\n(require 'cljs.test)\n"
                     "(cljs.test/run-block (concat (cljs.test/run-tests-block "
                     (mapconcat (lambda (test) (concat "'" (nth 1 test))) tests " ")
                     (format ") [(fn [] (println %S))]))" replique-cli--tests-done))
                    :cljs t :quiet t :on-out seen :timeout timeout)))
          (while (and ran (not finished) (< (float-time) deadline))
            (accept-process-output (replique-conn--proc control) 0.1))
          (cond ((not ran) nil)
                ((not finished)
                 (replique-cli-err (format "rq: the tests had not finished after %ss\n" timeout))
                 nil)
                (t (let ((start 0) (summaries 0) (bad 0))
                     (while (string-match "\\([0-9]+\\) failures, \\([0-9]+\\) errors"
                                          printed start)
                       (cl-incf summaries)
                       (cl-incf bad (+ (string-to-number (match-string 1 printed))
                                       (string-to-number (match-string 2 printed))))
                       (setq start (match-end 0)))
                     (and (> summaries 0) (zerop bad))))))
      (replique-conn-close control))))

(defun replique-cli-test (args)
  "Reload, load the test files, run them: .clj and .cljc in the JVM, .cljs in
the page.  Loaded rather than required: a test namespace that arrived by
require is outside what a reload watches, so its edits would never reach
the process, and outside what lints and usages answer."
  (pcase-let* ((`(,opts ,names) (replique-cli--options args))
               (info (replique-cli--process))
               (timeout (max 600 (plist-get opts :timeout)))
               (tests (mapcar (lambda (name)
                                (let* ((parts (split-string name "/"))
                                       (file (replique-cli--ns-file (car parts))))
                                  (unless file
                                    (replique-cli-stop 2 "rq: no file for %s under the project's classpath directories"
                                             (car parts)))
                                  (list name (car parts) (cadr parts) file)))
                              names))
               (cljs-p (lambda (test) (or (plist-get opts :cljs)
                                          (string-suffix-p ".cljs" (nth 3 test)))))
               (clj (seq-remove cljs-p tests))
               (cljs (seq-filter cljs-p tests)))
    (unless tests (replique-cli-stop 2 "rq: test what? rq test NS[/test-name]..."))
    (when (seq-some (lambda (test) (nth 2 test)) cljs)
      (replique-cli-stop 2 "rq: a single ClojureScript test is not supported - name its namespace"))
    (let ((clj-ok (or (null clj) (replique-cli--test-clj info clj timeout)))
          (cljs-ok (or (null cljs) (replique-cli--test-cljs info cljs timeout))))
      (and clj-ok cljs-ok))))

(defun replique-cli-lints (args)
  "The compiler's lints for files it has loaded."
  (pcase-let* ((`(,_ ,files) (replique-cli--options args))
               (info (replique-cli--process))
               (clean t))
    (unless files (replique-cli-stop 2 "rq: lint what? rq lints FILE..."))
    (dolist (f files)
      (dolist (dialect (replique-cli--dialects f))
        (let ((reply (condition-case err
                         (replique-cli--control info (append (list :op :lints :file (expand-file-name f))
                                                   (when (eq dialect 'cljs) '(:dialect :cljs))))
                       (replique-cli-refused (list :rq-failed (replique-cli--refusal-text (cadr err)))))))
          (cond ((plist-get reply :rq-failed)
                 (setq clean nil)
                 (replique-cli-out "%s [%s]: replique could not lint it - %s" f dialect
                         (plist-get reply :rq-failed)))
                ((not (plist-get reply :analysed))
                 (setq clean nil)
                 (replique-cli-out "%s [%s]: not analysed - the process never loaded it; `rq load` it first"
                         f dialect))
                ((plist-get reply :changed)
                 (setq clean nil)
                 (replique-cli-out "%s [%s]: changed on disk since it was compiled - `rq reload` first"
                         f dialect))
                (t (dolist (lint (plist-get reply :lints))
                     (setq clean nil)
                     (replique-cli-out "%s:%s:%s: %s [%s %s] %s" f
                             (plist-get lint :line) (plist-get lint :column)
                             (plist-get lint :level) dialect
                             (plist-get lint :type) (plist-get lint :message))))))))
    clean))

(defun replique-cli-usages (args)
  "Every use of a name, as a namespace writes it, from both compilers.
A .cljc var is two vars, one per compiler, and each is used from its own
world: asked of one, half the uses are missing."
  (pcase-let* ((`(,opts ,rest) (replique-cli--options args))
               (`(,ns ,text) (if (cdr rest)
                                 rest
                               ;; a qualified name is written that way in its own namespace
                               (list (car (split-string (or (car rest) "") "/")) (car rest))))
               (info (progn (unless (and ns text)
                              (replique-cli-stop 2 "rq: rq usages NS SYMBOL, or rq usages NS/VAR"))
                            (replique-cli--process)))
               (dialects (cond ((plist-get opts :cljs) '(cljs))
                               ((plist-get opts :clj) '(clj))
                               (t '(clj cljs))))
               (refusals nil)
               (answers (delq nil
                              (mapcar
                               (lambda (dialect)
                                 (let ((reply (condition-case err
                                                  (replique-cli--control
                                                   info
                                                   (append (list :op :usages :position :code
                                                                 :ns ns :text text)
                                                           (when (eq dialect 'cljs)
                                                             '(:dialect :cljs))))
                                                (replique-cli-refused
                                                 (push (format "%s: %s" dialect
                                                               (replique-cli--refusal-text (cadr err)))
                                                       refusals)
                                                 nil))))
                                   (when (plist-get reply :symbol)
                                     (plist-put reply :rq-dialect dialect))))
                               dialects))))
    (cond
     ;; Refused by every compiler asked, which is not the same answer as a
     ;; name that is nothing: a process that records nothing cannot say
     ((and (null answers) (= (length refusals) (length dialects)))
      (replique-cli-stop 2 "rq: %s" (string-join (nreverse refusals) "\n    ")))
     ((null answers)
      (replique-cli-out "nothing named %s in %s" text ns))
     (t
      (let* ((sym (plist-get (car answers) :symbol))
             (uses (delete-dups
                    (cl-loop for reply in answers
                             append (mapcar (lambda (u)
                                              (list (replique-cli--rel (plist-get u :file))
                                                    (plist-get u :line) (plist-get u :column)
                                                    (plist-get u :from-ns)))
                                            (plist-get reply :usages))))))
        (replique-cli-out "%s %s%s%s  [%s]" (plist-get sym :type)
                (if (plist-get sym :ns) (concat (plist-get sym :ns) "/") "")
                (plist-get sym :name)
                (if (plist-get sym :file)
                    (format "  defined %s:%s" (replique-cli--rel (plist-get sym :file)) (plist-get sym :line))
                  "")
                (mapconcat (lambda (r) (symbol-name (plist-get r :rq-dialect))) answers " "))
        (dolist (u (sort uses (lambda (a b) (or (string< (car a) (car b))
                                                (and (equal (car a) (car b))
                                                     (< (cadr a) (cadr b)))))))
          (apply #'replique-cli-out "%s:%s:%s  %s" u))
        (replique-cli-out "%d usages, in the namespaces this process has loaded" (length uses)))))
    t))

(defun replique-cli-main-js (args)
  "Write the module the page includes, naming this process's browser runtime.
Writing it starts that runtime when it is not up. The program it loads is
what the file already names unless --main says otherwise."
  (pcase-let* ((`(,opts ,files) (replique-cli--options args))
               (file (or (car files) (replique-cli-stop 2 "rq: write what? rq main-js FILE [--main NS]")))
               (main (or (plist-get opts :main) (replique-main-js--names file)))
               (reply (replique-cli--control (replique-cli--process)
                                   (append (list :op :main-js :file (expand-file-name file))
                                           (when main (list :main main)))
                                   (plist-get opts :timeout))))
    (replique-cli-out "%s connects to %s and loads %s"
            (replique-cli--rel (plist-get reply :file)) (plist-get reply :url)
            (or (plist-get reply :main) "nothing"))
    t))

(defconst replique-cli--formatted-extensions '("clj" "cljs" "cljc" "edn")
  "What `rq fmt' formats.")

(defun replique-cli--changed-files ()
  "The files git says changed in this project, staged, unstaged or new.
Relative to the project, which need not be the root of its repository."
  (let ((here default-directory)
        (default-directory (file-name-as-directory (or (replique-cli--project) default-directory))))
    (mapcar (lambda (f) (file-relative-name (expand-file-name f) here))
            (seq-filter #'file-regular-p
                        (delete-dups
                         (append (process-lines "git" "diff" "--name-only" "--relative" "HEAD")
                                 (process-lines "git" "ls-files" "--others" "--exclude-standard")))))))

(defun replique-cli-fmt (args)
  "Format files with replique and no process.
The project\='s cljfmt configuration is followed, as clojure-lsp reads it;
`.dir-locals.el' is not, since a formatter run outside Emacs does not read
it either."
  (pcase-let* ((`(,opts ,files) (replique-cli--options args))
               (check (plist-get opts :check))
               (files (or files
                          (seq-filter (lambda (f)
                                        (member (file-name-extension f)
                                                replique-cli--formatted-extensions))
                                      (replique-cli--changed-files))))
               (ok t))
    (unless files (replique-cli-out "no Clojure files changed"))
    (dolist (f files)
      (cond
       ((not (file-regular-p f)) (replique-cli-stop 2 "rq: no file %s" f))
       ((not (member (file-name-extension f) replique-cli--formatted-extensions))
        (replique-cli-out "%s: not Clojure, skipped" f))
       (t
        (let ((path (expand-file-name f)))
          (with-temp-buffer
            (insert-file-contents path)
            (setq default-directory (file-name-directory path))
            (let ((before (buffer-string)))
              (replique-clojure-mode)
              (condition-case err
                  (progn
                    (replique-format-buffer)
                    (unless (equal before (buffer-string))
                      (if check
                          (progn (setq ok nil) (replique-cli-out "%s: not formatted" f))
                        (let ((inhibit-message t))
                          (write-region nil nil path nil 'silent))
                        (replique-cli-out "%s: formatted" f))))
                (user-error (setq ok nil)
                            (replique-cli-out "%s: %s" f (error-message-string err))))))))))
    ok))

(defconst replique-cli-help "rq - the replique process that reads this project

  rq info                       which process reads this project
  rq stale                      what changed on disk since the process read it
  rq eval [--ns NS] CODE        evaluate Clojure
  rq eval --file F.clj          evaluate the forms of a scratch file, one by one
  rq eval --cljs CODE           evaluate ClojureScript in the connected page
  rq load FILE...               load files as units (.cljs files: --cljs implied)
  rq reload                     load every changed file, clj then cljs, then
                                build the stylesheets and reload them in the page
  rq css                        build the stylesheets and reload them in the page
  rq test NS[/test]...          reload, load the test files, run them
  rq lints FILE...              the compiler's lints for files it has loaded
  rq usages NS SYM              every use of SYM as NS writes it (str/join, ::k, Foo)
  rq usages NS/VAR              from clj and cljs both (--clj or --cljs for one)
  rq fmt [FILE...]              format per the project's cljfmt config (no files:
                                the git-changed ones)
  rq fmt --check [FILE...]      say what is not formatted, change nothing
  rq main-js FILE [--main NS]   write the module the page includes, pointing it at
                                this process (starts its browser runtime)

  --timeout S   seconds to wait (eval 120, reload 300, test 600)
  --full        print results without *print-length* / *print-level*

The process is the one running in this project - the nearest deps.edn above
here - or else one running where this project is read from elsewhere.

`rq fmt' needs no process.

Exit status: 0 ok; 1 a form threw, tests failed, lints found, files not
formatted; 2 rq could not do it; 3 no process reads this project - ask the
user.")

(defun replique-cli-main ()
  "Run the command on the command line, and exit with its status.
The file $REPLIQUE_CLI_INIT names, when it does, is loaded first."
  (when-let* ((init (getenv "REPLIQUE_CLI_INIT")))
    (unless (string-empty-p init) (load init nil t)))
  (let* ((args (if (equal (car command-line-args-left) "--")
                   (cdr command-line-args-left)
                 command-line-args-left))
         (command (car args))
         (status
          (condition-case err
              (let ((ok (pcase command
                          ("info" (replique-cli-info (cdr args)))
                          ("stale" (replique-cli-stale (cdr args)))
                          ("eval" (replique-cli-eval (cdr args)))
                          ("load" (replique-cli-load (cdr args)))
                          ("reload" (replique-cli-reload (cdr args)))
                          ("css" (replique-cli-css (cdr args)))
                          ("test" (replique-cli-test (cdr args)))
                          ("lints" (replique-cli-lints (cdr args)))
                          ("usages" (replique-cli-usages (cdr args)))
                          ("fmt" (replique-cli-fmt (cdr args)))
                          ("main-js" (replique-cli-main-js (cdr args)))
                          (_ (replique-cli-out "%s" replique-cli-help) t))))
                (if ok 0 1))
            (replique-cli-stop (replique-cli-err (concat (nth 2 err) "\n")) (nth 1 err))
            (replique-cli-refused (replique-cli-err (format "rq: %s\n" (replique-cli--refusal-text (cadr err)))) 2))))
    (setq command-line-args-left nil)
    (kill-emacs status)))

(provide 'replique-cli)

;;; replique-cli.el ends here
