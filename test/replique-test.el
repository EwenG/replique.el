;;; replique-test.el --- Tests, against a real process  -*- lexical-binding: t; -*-

;;; Commentary:

;; The client is tested against a replique process, not against a mock of
;; one: what is worth checking here is the reading of frames a real process
;; produces, and a mock would only test the mock.
;;
;; Run them with the Clojure project that provides replique:
;;
;;   REPLIQUE_PROJECT=/path/to/replique emacs -Q -batch -L . -L test \
;;     -l replique-test -f ert-run-tests-batch-and-exit
;;
;; Without REPLIQUE_PROJECT the tests that need a process are skipped.
;;
;; One process is shared by the tests that only read from it - a jvm takes
;; seconds to start - and the tests that need one of their own say so.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique)

(defvar replique-test-process nil
  "The process shared by the tests.")

(defun replique-test-project ()
  "Return the Clojure project to start a process in, or skip the test.

An empty REPLIQUE_PROJECT is no project rather than the current
directory, which is what `expand-file-name\=' would make of it: the
makefile passes the variable through whether it was set or not, and the
tests that need a process would otherwise start one in the checkout they
are being run from - taking the name of the process the developer has
open on it."
  (let ((project (getenv "REPLIQUE_PROJECT")))
    (when (or (null project) (string-empty-p (string-trim project)))
      (ert-skip "REPLIQUE_PROJECT is not set"))
    (file-name-as-directory (expand-file-name project))))

(defun replique-test-wait-for (pred &optional timeout)
  "Pump until PRED holds or TIMEOUT seconds pass.  Returns what PRED said."
  (let ((limit (+ (float-time) (or timeout 30))))
    (while (and (not (funcall pred)) (< (float-time) limit))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defun replique-test-settle (&optional seconds)
  "Pump for SECONDS, to let what is not coming arrive."
  (let ((limit (+ (float-time) (or seconds 0.6))))
    (while (< (float-time) limit) (accept-process-output nil 0.05))))

(defun replique-test-start (&optional options)
  "Start a process in the test project and wait for it.

OPTIONS is what to pass to replique.main, defaulting to what
`replique-start' passes.  Returns the replique process when Emacs started
it, and the operating system process when OPTIONS asked for one Emacs
knows nothing about - a test that is given nothing cannot tell a process
that went from one that was never there."
  (let* ((project (replique-test-project))
         (default-directory project)
         (known replique-processes))
    (if options
        (make-process :name "replique-test" :buffer nil
                      :command (list replique-clojure-program "-M" "-m" "replique.main" options)
                      :coding 'utf-8-unix :noquery t)
      (progn
        (replique-start project)
        (unless (replique-test-wait-for
                 (lambda () (seq-difference replique-processes known)) 120)
          (error "The process did not start"))
        (car (seq-difference replique-processes known))))))

(defun replique-test-cleanup ()
  "Stop every process the tests are still connected to.

Emacs no longer takes them with it: a process is started under nohup so
that it lives through the editor leaving, which a run of the tests has to
undo itself.  A process that survives holds the pipe it was started with,
and whatever reads that pipe waits for it - a test run piped into anything
would never end."
  (dolist (process (replique-processes-live))
    (replique-kill-process process)))

(add-hook 'kill-emacs-hook #'replique-test-cleanup)

(defun replique-test-process ()
  "Return the process the tests share, started on first use."
  (unless (replique-process-live-p replique-test-process)
    (setq replique-test-process (replique-test-start)))
  replique-test-process)

(defun replique-test-repl ()
  "Return a repl on the shared process, ready to be written to."
  (let ((repl (replique-repl (replique-test-process))))
    (unless (replique-test-wait-for (lambda () (replique-repl--ns repl)) 30)
      (error "The repl did not answer"))
    repl))

(defun replique-test-text (repl)
  "Return what the buffer of REPL holds."
  (with-current-buffer (replique-repl--buffer repl)
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro replique-test-with-repl (name &rest body)
  "Run BODY with NAME bound to a repl of the shared process."
  (declare (indent 1))
  `(let ((,name (replique-test-repl)))
     (unwind-protect (progn ,@body)
       (replique-conn-close (replique-repl--conn ,name))
       (kill-buffer (replique-repl--buffer ,name)))))

(defun replique-test-eval (repl code)
  "Evaluate CODE in REPL and return what the buffer gained by it.

What the buffer already held is left out on purpose: an assertion made
against the whole buffer can be answered by the code the test itself just
echoed into it, which is how a broken source directive passed for a
while."
  (let ((before (replique-test-text repl)))
    (replique-repl-send-code repl code)
    (replique-test-wait-for
     (lambda () (and (replique-repl--at-prompt repl)
                     (not (equal before (replique-test-text repl))))))
    (substring (replique-test-text repl) (length before))))

(defun replique-test-hide (buffer)
  "Make sure no window shows BUFFER.

`replique-repl\=' shows the buffer it opened, and what a buffer no window
shows is told about is the point of half of these tests."
  (dolist (window (get-buffer-window-list buffer nil t))
    (set-window-buffer window (get-buffer-create "*scratch*"))))

(defvar replique-test--message nil
  "The last message the code under test produced.")

(defmacro replique-test-message (&rest body)
  "Run BODY and return the last message it put in the echo area."
  (declare (indent 0))
  `(let ((replique-test--message nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args)
                  (setq replique-test--message (apply #'format format args)))))
       ,@body)
     replique-test--message))

;;; Printing EDN

(ert-deftest replique-test-edn-scalars ()
  (should (equal "nil" (replique-edn-print nil)))
  (should (equal "true" (replique-edn-print t)))
  (should (equal "false" (replique-edn-print 'false)))
  (should (equal ":op" (replique-edn-print :op)))
  (should (equal "42" (replique-edn-print 42)))
  (should (equal "\"a\"" (replique-edn-print "a"))))

(ert-deftest replique-test-edn-strings-fit-on-one-line ()
  "A control connection reads one message per line, so a newline inside a
string must be escaped rather than written."
  (should (equal "\"a\\nb\"" (replique-edn-print "a\nb")))
  (should (equal "\"a\\\"b\"" (replique-edn-print "a\"b")))
  (should (equal "\"a\\\\b\"" (replique-edn-print "a\\b")))
  (should-not (string-search "\n" (replique-edn-map (list :file "a\nb")))))

(ert-deftest replique-test-edn-maps ()
  (should (equal "{:op :hello :role :control}"
                 (replique-edn-map (list :op :hello :role :control))))
  (should (equal "{}" (replique-edn-map nil))))

(ert-deftest replique-test-process-output-is-coloured ()
  "The output buffer has no font lock, so a `font-lock-face' would simply
not be honoured there."
  (let ((process (replique-process--make :id "faces")))
    (unwind-protect
        (progn
          (replique-process--insert process "went wrong\n" 'replique-stderr)
          (with-current-buffer (replique-process-buffer process)
            (goto-char (point-min))
            (should (eq 'replique-stderr (get-text-property (point) 'face)))))
      (kill-buffer (replique-process-buffer process)))))

(ert-deftest replique-test-a-missing-clojure-says-which-setting-to-look-at ()
  (let ((replique-clojure-program "replique-no-such-program"))
    (should-error (replique-start temporary-file-directory) :type 'user-error)))

(defconst replique-test-exception
  '(:class "clojure.lang.ExceptionInfo"
    :message "could not read the config"
    :data "{:file \"conf.edn\"}"
    :trace ("user$eval1.invokeStatic(NO_SOURCE_FILE:1)"
            "clojure.lang.Compiler.eval(Compiler.java:7757)"
            "replique.repl$repl.invokeStatic(repl.clj:232)")
    :trace-dropped 61
    :cause (:class "java.lang.NumberFormatException"
            :message "For input string: \"x\""
            :trace ("java.base/java.lang.NumberFormatException.forInputString(NumberFormatException.java:67)"
                    "java.base/java.lang.Integer.parseInt(Integer.java:565)"
                    "my.app$parse.invokeStatic(core.clj:12)"
                    "clojure.lang.AFn.run(AFn.java:22)"
                    "java.base/java.lang.Thread.run(Thread.java:1474)")
            :cause-dropped t))
  "An exception of the shape a frame carries, to render without a process.")

(defun replique-test-rendered (&rest body-fns)
  "Render the fixture, run BODY-FNS in its buffer, and return what it says."
  (let ((buffer (replique-exception-show replique-test-exception
                                         "Execution error (NumberFormatException)."
                                         "execution" "at the repl")))
    (unwind-protect
        (with-current-buffer buffer
          (dolist (f body-fns) (funcall f))
          (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer buffer))))

(ert-deftest replique-test-a-chain-is-outermost-first ()
  (let ((chain (replique-exception-chain replique-test-exception)))
    (should (= 2 (length chain)))
    (should (equal "clojure.lang.ExceptionInfo" (plist-get (car chain) :class)))
    (should (equal "java.lang.NumberFormatException" (plist-get (cadr chain) :class)))))

(ert-deftest replique-test-the-viewer-says-what-was-left-out ()
  "A frame carries the top of a trace and the outermost causes.  What it
left out is written where it was left out, or a cut exception reads as a
whole one."
  (let ((text (replique-test-rendered)))
    (should (string-match-p "2 causes, and the chain goes on" text))
    (should (string-match-p "3 of 64 frames carried" text))
    (should (string-match-p "61 frames the frame did not carry" text))
    (should (string-match-p "the chain goes on below what the frame carried" text))
    ;; the chain was cut, so the root is not among the causes carried
    (should-not (string-match-p "  root$" text))))

(ert-deftest replique-test-the-viewer-marks-the-root ()
  "The message a repl reports is the root cause's, so a reader has to be
able to see which one that is."
  (let* ((whole (append (butlast replique-test-exception 0) nil))
         (replique-test-exception
          (plist-put (copy-sequence replique-test-exception) :cause
                     (plist-put (copy-sequence (plist-get replique-test-exception :cause))
                                :cause-dropped nil))))
    (ignore whole)
    (should (string-match-p "  root$" (replique-test-rendered)))))

(ert-deftest replique-test-folding-keeps-the-throw-path ()
  "Where a jdk exception was thrown is jdk frames, and folding the runtime
away has to keep them: `Integer.parseInt' is the answer, `Thread.run' is
not, and no rule about package names tells them apart."
  (let ((text (replique-test-rendered #'replique-exception-next-cause
                                      #'replique-exception-toggle-runtime-frames)))
    (should (string-match-p "NumberFormatException.forInputString" text))
    (should (string-match-p "Integer.parseInt" text))
    (should (string-match-p "my.app\\$parse" text))
    (should-not (string-match-p "Thread.run" text))
    (should-not (string-match-p "AFn.run" text))
    ;; and it says so, so that a folded trace is never taken for a whole one
    (should (string-match-p "2 runtime frames folded" text))))

(ert-deftest replique-test-a-start-that-failed-carries-its-exception ()
  "Replique reports a start it could not finish as an exception like any
other, so the buffer offers the same way into it."
  (let* ((proc (make-process :name "replique-test" :buffer nil
                             :command (list "cat") :noquery t))
         (buffer (generate-new-buffer "*replique-test-start*")))
    (unwind-protect
        (progn
          (process-put proc 'replique-buffer buffer)
          (process-put proc 'replique-state 'starting)
          (cl-letf (((symbol-function 'display-buffer) #'ignore))
            ;; the line as it comes off the wire
            (replique-process--spawn-filter
             proc (concat "{\"tag\":\"error\",\"error\":\"start-failed\""
                          ",\"message\":\"Invalid port: 99999\""
                          ",\"exception\":{\"class\":\"clojure.lang.ExceptionInfo\""
                          ",\"message\":\"Invalid port: 99999\""
                          ",\"trace\":[\"replique.core$validate_port.invokeStatic(core.clj:36)\"]}}"
                          "\n")))
          (should (eq 'failed (process-get proc 'replique-state)))
          (with-current-buffer buffer
            (should (string-match-p "browse the exception" (buffer-string)))
            ;; and nothing of replique's own beyond that: what the buffer
            ;; holds is what the process wrote
            (should-not (string-match-p "Invalid port" (buffer-string)))))
      (delete-process proc)
      (kill-buffer buffer))))

(ert-deftest replique-test-the-editor-brings-replique-along ()
  "A project needs no change to be worked on: replique goes on the
classpath beside its dependencies rather than into them."
  (let ((replique-coordinates "{:local/root \"/tmp/replique\"}")
        (replique-aliases nil)
        (replique-clojure-program "clojure"))
    (should (equal (append (when (executable-find "nohup") '("nohup"))
                           '("clojure"
                             "-Sdeps" "{:deps {replique/replique {:local/root \"/tmp/replique\"}}\n}"
                             "-M" "-m" "replique.main"
                             "{:process-id \"a-project\"}"))
                   (replique-process--command "/tmp/a-project/")))))

(ert-deftest replique-test-a-project-that-has-replique-is-left-alone ()
  (let ((replique-coordinates nil)
        (replique-aliases nil)
        (replique-clojure-program "clojure"))
    (should-not (member "-Sdeps" (replique-process--command "/tmp/a-project/")))))

(ert-deftest replique-test-aliases-reach-the-command ()
  "Sources behind an alias are sources that need the alias to be there."
  (let ((replique-clojure-program "clojure")
        (replique-coordinates nil))
    (let ((replique-aliases '("dev")))
      (should (member "-M:dev" (replique-process--command "/tmp/a-project/"))))
    ;; written with or without the colon, since both read as the alias
    (let ((replique-aliases '(":dev" "test")))
      (should (member "-M:dev:test" (replique-process--command "/tmp/a-project/"))))))

(ert-deftest replique-test-your-aliases-are-added-to-the-projects ()
  "Tooling of your own is named in your init and lives in your deps.edn,
so a project that needs an alias to be usable still gets it and nothing
about yours reaches the project."
  (let ((replique-clojure-program "clojure")
        (replique-coordinates nil)
        (replique-aliases '("dev"))
        (replique-user-aliases '("my-tools")))
    (should (member "-M:dev:my-tools" (replique-process--command "/tmp/a-project/"))))
  (let ((replique-clojure-program "clojure")
        (replique-coordinates nil)
        (replique-aliases nil)
        (replique-user-aliases '("my-tools")))
    (should (member "-M:my-tools" (replique-process--command "/tmp/a-project/"))))
  ;; the same alias named twice is the same alias
  (let ((replique-clojure-program "clojure")
        (replique-coordinates nil)
        (replique-aliases '("dev"))
        (replique-user-aliases '(":dev")))
    (should (member "-M:dev" (replique-process--command "/tmp/a-project/")))))

(ert-deftest replique-test-aliases-come-from-the-project ()
  "Which aliases a project needs is a property of the project, so they are
read where the project is - not where the command was called from."
  (let ((dir (file-name-as-directory (make-temp-file "replique-test-locals" t)))
        (replique-aliases '("whatever-the-caller-had")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name ".dir-locals.el" dir)
            (insert "((nil . ((replique-aliases . (\"dev\")))))\n"))
          (should (equal '("dev") (replique-process--project-aliases dir)))
          ;; and a project that asks for nothing leaves the caller alone
          (delete-file (expand-file-name ".dir-locals.el" dir))
          (should (equal '("whatever-the-caller-had")
                         (replique-process--project-aliases dir))))
      (delete-directory dir t))))

(defmacro replique-test-with-project (name &rest body)
  "Run BODY with NAME bound to an empty project directory."
  (declare (indent 1))
  `(let ((,name (file-name-as-directory (make-temp-file "replique-test-project" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,name t))))

(defun replique-test-write-aliases (directory text)
  "Write TEXT as the aliases of your own kept in DIRECTORY."
  (let ((file (expand-file-name replique-aliases-file directory)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert text))))

(ert-deftest replique-test-aliases-of-your-own-reach-the-deps ()
  "Tooling that is yours but belongs to one project has nowhere good to
go: the deps.edn of the project is shared, and yours is not about this
project.  A file in the project that its version control ignores is."
  (replique-test-with-project dir
    (let ((replique-coordinates "{:local/root \"/tmp/replique\"}"))
      (replique-test-write-aliases
       dir "{:mine {:extra-deps {org.clojure/data.json {:mvn/version \"2.5.1\"}}}}")
      (should (equal (concat "{:deps {replique/replique {:local/root \"/tmp/replique\"}}"
                             " :aliases {:mine {:extra-deps"
                             " {org.clojure/data.json {:mvn/version \"2.5.1\"}}}}"
                             "\n}")
                     (replique-process--sdeps dir))))))

(ert-deftest replique-test-a-comment-does-not-swallow-the-brace ()
  "The aliases are spliced in as they were written, comments and all, so
what closes the map around them cannot share their last line."
  (replique-test-with-project dir
    (let ((replique-coordinates nil))
      (replique-test-write-aliases dir "{:mine {:extra-paths [\"dev\"]}} ; mine\n")
      (let ((sdeps (replique-process--sdeps dir)))
        (should (string-suffix-p "\n}" sdeps))
        (should (string-match-p "; mine" sdeps))))))

(ert-deftest replique-test-a-project-with-nothing-of-yours-in-it ()
  (replique-test-with-project dir
    (let ((replique-coordinates "{:local/root \"/tmp/replique\"}"))
      (should (equal "{:deps {replique/replique {:local/root \"/tmp/replique\"}}\n}"
                     (replique-process--sdeps dir))))
    (let ((replique-coordinates nil))
      (should-not (replique-process--sdeps dir)))))

;;; The directory a command is about

(defmacro replique-test-in-directory (directory &rest body)
  "Run BODY as a buffer in DIRECTORY would, making it first."
  (declare (indent 1))
  `(let ((default-directory (file-name-as-directory ,directory)))
     (make-directory default-directory t)
     ,@body))

(defmacro replique-test-with-process-in (directory &rest body)
  "Run BODY with a process of DIRECTORY in the registry.

Made rather than started: what is asked of it is its directory, and a
process that is there is all the guess wants to know."
  (declare (indent 1))
  `(let ((replique-processes
          (list (replique-process--make :id "taken" :directory ,directory))))
     (cl-letf (((symbol-function 'replique-process-live-p) (lambda (_process) t)))
       ,@body)))

(ert-deftest replique-test-the-nearest-deps-edn-is-the-project ()
  "The module rather than the repository holding it: a deps.edn is what
clojure reads, so the nearest one is where a process can run."
  (replique-test-with-project dir
    (with-temp-file (expand-file-name "deps.edn" dir) (insert "{}"))
    (make-directory (expand-file-name "mod" dir))
    (with-temp-file (expand-file-name "mod/deps.edn" dir) (insert "{}"))
    (replique-test-in-directory (expand-file-name "mod/src/deep" dir)
      (should (equal (expand-file-name "mod/" dir)
                     (replique-process--directory-to-start)))
      (should (equal (expand-file-name "mod/" dir)
                     (replique-process--directory-to-connect))))))

(ert-deftest replique-test-a-project-emacs-knows-stands-in-for-a-missing-deps-edn ()
  (replique-test-with-project dir
    (make-directory (expand-file-name ".git" dir))
    (replique-test-in-directory (expand-file-name "src" dir)
      (should (equal dir (replique-process--directory-to-start))))))

(ert-deftest replique-test-a-directory-that-is-taken-is-not-proposed-for-a-start ()
  "A repl buffer is in the directory of its own process, which is the one
directory `replique-start' refuses.  What is proposed there is the project
above it."
  (replique-test-with-project dir
    (with-temp-file (expand-file-name "deps.edn" dir) (insert "{}"))
    (make-directory (expand-file-name "mod" dir))
    (with-temp-file (expand-file-name "mod/deps.edn" dir) (insert "{}"))
    (replique-test-with-process-in (expand-file-name "mod/" dir)
      (replique-test-in-directory (expand-file-name "mod" dir)
        (should (equal dir (replique-process--directory-to-start)))))))

(ert-deftest replique-test-a-connect-is-proposed-the-directory-of-the-port-file ()
  "Not the project root: a process is found where it was started, and that
is only usually the same directory."
  (replique-test-with-project dir
    (with-temp-file (expand-file-name "deps.edn" dir) (insert "{}"))
    (let ((file (expand-file-name "mod/.replique/processes/mod.json" dir)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert (json-serialize (list :process-id "mod" :host "127.0.0.1"
                                      :port 1 :pid 999999 :started-at 1)))))
    (replique-test-in-directory (expand-file-name "mod/src" dir)
      (should (equal (expand-file-name "mod/" dir)
                     (replique-process--directory-to-connect)))
      ;; The project is still what it was: only the connect follows the file
      (should (equal dir (replique-process--directory-to-start))))))

;;; The handshake

(ert-deftest replique-test-a-process-describes-itself ()
  (let ((process (replique-test-process)))
    (should (replique-process--id process))
    (should (plist-get (replique-process--info process) :clojure-version))
    (should (plist-get (replique-process--info process) :port))))

(ert-deftest replique-test-the-control-connection-answers-a-request ()
  (let ((process (replique-test-process))
        (answer nil))
    (replique-process-request process (list :op :process-info)
                              (lambda (frame) (setq answer frame)))
    (should (replique-test-wait-for (lambda () answer)))
    (should (equal (replique-process--id process) (plist-get answer :process-id)))))

(ert-deftest replique-test-connecting-to-a-process-emacs-started-is-that-process ()
  (let* ((process (replique-test-process))
         (known replique-processes))
    (setq replique-current-process nil)
    (replique-connect (replique-process--directory process))
    ;; A connection that was opened would register on its handshake rather
    ;; than now, so what says none was is that nothing arrives
    (replique-test-settle)
    (should (equal known replique-processes))
    (should (eq process replique-current-process))))

(ert-deftest replique-test-a-second-process-in-one-directory-is-refused ()
  (let ((process (replique-test-process))
        (buffers (match-buffers "\\`\\*replique-process: ")))
    (should-error (replique-start (replique-process--directory process))
                  :type 'user-error)
    ;; Refused before anything was made: a command that failed leaves no
    ;; buffer of a process that was never started
    (should (equal buffers (match-buffers "\\`\\*replique-process: ")))))

(ert-deftest replique-test-a-start-reaps-before-it-spawns ()
  "A crash leaves a port file, and the process will not start where there is
one.  `true\=' stands in for clojure: what is asserted is what the command did
before it spawned anything."
  (let* ((dir (file-name-as-directory (make-temp-file "replique-test" t)))
         (file (expand-file-name ".replique/processes/gone.json" dir))
         (replique-clojure-program "true")
         (proc nil))
    (unwind-protect
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            ;; Port 1, which nothing listens on
            (insert (json-serialize (list :process-id "gone" :host "127.0.0.1"
                                          :port 1 :pid 999999 :started-at 1))))
          (setq proc (replique-start dir))
          (should-not (file-exists-p file)))
      (when proc
        (when (buffer-live-p (replique-process--startup-buffer proc))
          (kill-buffer (replique-process--startup-buffer proc)))
        (delete-process proc))
      (delete-directory dir t))))

(ert-deftest replique-test-a-start-keeps-the-port-file-of-a-process-that-is-there ()
  (let* ((process (replique-test-process))
         (file (car (car (replique-process-descriptions
                          (replique-process--directory process))))))
    (should file)
    (replique-process--reap-directory (replique-process--directory process))
    (should (file-exists-p file))))

;;; A repl

(ert-deftest replique-test-a-form-produces-a-result ()
  (replique-test-with-repl repl
    (should (equal "user" (replique-repl--ns repl)))
    (should (string-match-p "^42$" (replique-test-eval repl "(+ 40 2)")))))

(ert-deftest replique-test-output-comes-before-the-result ()
  (replique-test-with-repl repl
    (should (string-match-p "line 0\nline 1\nnil"
                            (replique-test-eval repl "(dotimes [i 2] (println \"line\" i))")))))

(ert-deftest replique-test-a-prompt-does-not-mean-a-form-was-answered ()
  "A read error and a line holding only a comment produce a prompt of
their own, so a client that writes one prompt per prompt frame ends up
with a buffer full of them."
  (replique-test-with-repl repl
    (let ((before (replique-test-text repl)))
      (replique-conn-send-code (replique-repl--conn repl) ";; just a comment")
      (replique-test-settle)
      (should (equal before (replique-test-text repl))))))

(ert-deftest replique-test-the-prompt-says-the-namespace ()
  (replique-test-with-repl repl
    (replique-test-eval repl "(in-ns 'brand.new)")
    (should (equal "brand.new" (replique-repl--ns repl)))
    (should (string-match-p "^brand.new=> " (replique-test-text repl)))))

(ert-deftest replique-test-an-exception-is-reported-as-a-terminal-repl-reports-it ()
  (replique-test-with-repl repl
    (let ((text (replique-test-eval repl "(throw (ex-info \"boom\" {:a 1}))")))
      (should (string-match-p "Execution error (ExceptionInfo)" text))
      (should (string-match-p "boom" text)))
    (let ((exception (plist-get (replique-repl--last-exception repl) :exception)))
      (should (equal "clojure.lang.ExceptionInfo" (plist-get exception :class)))
      (should (plist-get exception :trace)))))

(ert-deftest replique-test-a-cut-trace-is-shown-as-cut ()
  "A frame carries the top of the trace and the outermost causes.  What it
left out has to be said: the root cause is the one the reported message
names, and 64 of 300 frames shown as a whole trace is a lie."
  (replique-test-with-repl repl
    (should (string-match-p "more frames"
                            (replique-test-eval repl "((fn f [n] (inc (f (inc n)))) 0)")))
    (should (plist-get (plist-get (replique-repl--last-exception repl) :exception)
                       :trace-dropped))))

(ert-deftest replique-test-nothing-is-said-about-what-was-not-cut ()
  (replique-test-with-repl repl
    (replique-test-eval repl "(throw (Exception. \"shallow\"))")
    (let ((exception (plist-get (replique-repl--last-exception repl) :exception)))
      (should-not (plist-get exception :trace-dropped))
      (should-not (plist-get exception :cause-dropped)))))

(ert-deftest replique-test-what-is-typed-at-the-prompt-is-evaluated ()
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(* 6 7)")
      (comint-send-input))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^42$" (replique-test-text repl)))))))

(ert-deftest replique-test-whether-what-is-typed-is-a-form-yet ()
  "Reading enough of Clojure to know where a form ends, which is what
tells a newline to send from a newline to insert."
  (with-temp-buffer
    (cl-flet ((unfinished (text)
                (erase-buffer)
                (insert text)
                (replique-repl--unfinished-p (point-min) (point-max))))
      (should-not (unfinished "(+ 1 1)"))
      (should-not (unfinished "[1 2] {:a 1} #{3}"))
      (should (unfinished "(+ 1"))
      (should (unfinished "(let [a 1]\n  (+ a"))
      (should (unfinished "\"not closed"))
      (should-not (unfinished "\"a string with ( in it\""))
      (should-not (unfinished "#\"a regex with ( in it\""))
      ;; A character literal is an escape, and a comment is a comment
      (should-not (unfinished "\\("))
      (should-not (unfinished "; a comment with ( in it"))
      (should-not (unfinished "(+ 1 1) ; and one after a form"))
      ;; Closing what was never opened is wrong rather than unfinished, and
      ;; waiting for it to be finished would be waiting forever
      (should-not (unfinished "(+ 1 1))")))))

(ert-deftest replique-test-return-waits-for-the-form-to-be-finished ()
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(+ 1")
      (replique-repl-return))
    (replique-test-settle)
    (should (string-suffix-p "(+ 1\n" (replique-test-text repl)))
    (with-current-buffer (replique-repl--buffer repl)
      (insert " 1)")
      (replique-repl-return))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^2$" (replique-test-text repl)))))))

(ert-deftest replique-test-return-sends-what-is-not-a-form-when-told-to ()
  "Where the text ends is a guess about text nothing has read yet.  What
is sent anyway is read as the beginning of a form, and the line after it
finishes it: the repl is no longer at a prompt, so nothing is guessed
about that one."
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(+ 1")
      (replique-repl-return t)
      (insert " 1)")
      (replique-repl-return))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^2$" (replique-test-text repl)))))))

(ert-deftest replique-test-what-a-running-form-reads-is-not-read-as-a-form ()
  "A repl hands the code it evaluates a real stdin.  What is typed to a
form that is running is a line of text, finished when the developer says
it is - not when a delimiter closes."
  (replique-test-with-repl repl
    (replique-repl-send-code repl "(read-line)")
    (should (replique-test-wait-for
             (lambda () (not (replique-repl--at-prompt repl)))))
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "hello (")
      (replique-repl-return))
    (should (replique-test-wait-for
             (lambda () (string-match-p (regexp-quote "\"hello (\"")
                                        (replique-test-text repl)))))))

(ert-deftest replique-test-the-transcript-follows-the-repl-not-the-editor ()
  "Two forms sent back to back are answered one at a time.  Writing the
second one as soon as it is sent puts it above the result of the first,
which is a lie about what happened."
  (replique-test-with-repl repl
    (replique-repl-send-code repl "(+ 1 1)")
    (replique-repl-send-code repl "(+ 2 2)")
    (should (replique-test-wait-for
             (lambda () (string-match-p "^4$" (replique-test-text repl)))))
    (let ((text (replique-test-text repl)))
      (should (string-match-p (concat (regexp-quote "(+ 1 1)") "\n2\n[^\n]*=> "
                                      (regexp-quote "(+ 2 2)") "\n4")
                              text)))))

;;; Where the code comes from

(ert-deftest replique-test-the-compiler-is-told-where-the-code-came-from ()
  "A repl reads from a socket, so the file and the line numbers it records
mean nothing unless the client says where the form was taken from."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique-test-source.clj" temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";; a comment\n;; another\n(defn from-a-buffer [] :yes)\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (emacs-lisp-mode)   ; sexp motion is all this needs
                    (setq replique-current-repl repl)
                    (replique-eval-buffer))
                (kill-buffer buffer)))
            (replique-test-wait-for
             (lambda () (string-match-p "#'user/from-a-buffer" (replique-test-text repl))))
            ;; Read one key at a time: a result that is only a number cannot
            ;; be confused with the directive that was echoed above it
            (should (string-match-p "^3$" (replique-test-eval
                                           repl "(:line (meta #'from-a-buffer))")))
            (should (string-match-p (concat "^" (regexp-quote (format "\"%s\"" file)) "$")
                                    (replique-test-eval
                                     repl "(:file (meta #'from-a-buffer))"))))
        (delete-file file)))))

(ert-deftest replique-test-every-form-of-a-region-says-where-it-came-from ()
  "The directive applies to the next form only, so a region holding
several forms needs one before each: they would otherwise all be recorded
at the line of the first."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(def a 1)\n\n(def b 2)\n")
    (let ((forms (replique-eval--forms (point-min) (point-max))))
      (should (equal '(("(def a 1)" . 1) ("(def b 2)" . 3)) forms)))))

(ert-deftest replique-test-a-comment-between-two-forms-is-not-a-form ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert ";; a comment\n(def a 1)\n")
    (should (equal '(("(def a 1)" . 2))
                   (replique-eval--forms (point-min) (point-max))))))

(ert-deftest replique-test-the-transcript-does-not-show-the-source-directive ()
  "The directive is protocol.  Nobody wrote it, so a transcript that shows
it is a transcript of the wire rather than of the session."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique-test-directive.clj" temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "(def marker :here)\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (emacs-lisp-mode)
                    (setq replique-current-repl repl)
                    (replique-eval-buffer))
                (kill-buffer buffer)))
            (should (replique-test-wait-for
                     (lambda ()
                       (string-match-p "#'user/marker" (replique-test-text repl)))))
            (let ((text (replique-test-text repl)))
              (should (string-match-p "(def marker :here)" text))
              (should-not (string-match-p "replique/src" text))))
        (delete-file file)))))

(ert-deftest replique-test-multibyte-survives-the-round-trip ()
  "Everything is utf-8, in both directions.  The assertion is made on text
the form produced rather than on text it was given, so a round trip that
lost something cannot pass by echoing the question back."
  (replique-test-with-repl repl
    (let ((added (replique-test-eval
                  repl "(str (.toUpperCase \"café\") (apply str (repeat 2 \"🎉\")))")))
      (should (string-match-p "\"CAFÉ🎉🎉\"" added)))))

;;; Interrupting

(ert-deftest replique-test-an-evaluation-can-be-interrupted ()
  (replique-test-with-repl repl
    (replique-repl-send-code repl "(Thread/sleep 60000)")
    (replique-test-settle 0.4)
    (with-current-buffer (replique-repl--buffer repl)
      (replique-interrupt))
    (should (replique-test-wait-for
             (lambda () (string-match-p "InterruptedException" (replique-test-text repl)))
             15))))

(ert-deftest replique-test-an-idle-repl-is-left-alone ()
  "Interrupting a repl that is waiting for the next form would break the
connection rather than an evaluation."
  (replique-test-with-repl repl
    (let ((answer nil))
      (replique-process-request
       (replique-repl--process repl)
       (list :op :interrupt :connection (replique-conn--id (replique-repl--conn repl)))
       (lambda (frame) (setq answer frame)))
      (should (replique-test-wait-for (lambda () answer)))
      (should-not (eq t (plist-get answer :interrupted))))))

;;; Several repls

(ert-deftest replique-test-repls-are-independent ()
  (replique-test-with-repl one
    (replique-test-with-repl two
      (replique-test-eval two "(in-ns 'other.ns)")
      (should (equal "user" (replique-repl--ns one)))
      (should (equal "other.ns" (replique-repl--ns two)))
      (should-not (equal (replique-conn--id (replique-repl--conn one))
                         (replique-conn--id (replique-repl--conn two)))))))

;;; Where output goes

(ert-deftest replique-test-a-future-prints-in-the-repl-it-was-started-from ()
  "A future conveys the bindings of the repl, so what it prints is that
repl's output."
  (replique-test-with-repl repl
    (should (string-match-p
             "FROM-A-FUTURE"
             (replique-test-eval repl "(do @(future (println \"FROM-A-FUTURE\")) :done)")))))

(ert-deftest replique-test-what-belongs-to-no-repl-goes-to-the-process ()
  "A plain thread conveys nothing, so what it prints is the output of the
process.  A developer who cannot find that buffer will think it vanished."
  (let ((process (replique-test-process)))
    (replique-test-with-repl repl
      (replique-test-eval
       repl "(do (.start (Thread. (fn [] (println \"FROM-A-THREAD\")))) :started)")
      (should (replique-test-wait-for
               (lambda ()
                 (with-current-buffer (replique-process-buffer process)
                   (string-match-p "^FROM-A-THREAD$" (buffer-string))))
               10))
      (should-not (string-match-p "^FROM-A-THREAD$" (replique-test-text repl))))))

;;; Output nothing has seen

(ert-deftest replique-test-what-a-form-printed-is-echoed-with-its-result ()
  "A result read without the printing that went with it is half of what
happened, and the other half is in a buffer the developer is not looking
at - which is the whole reason the result was echoed in the first place."
  (replique-test-with-repl repl
    (let ((echoed (replique-test-message
                    (replique-repl-send-code
                     repl "(do (println \"PRINTED\") :returned)" nil t)
                    ;; the prompt rather than the text: the code the buffer
                    ;; was sent is echoed into it, and holds the answer
                    (replique-test-wait-for
                     (lambda () (replique-repl--at-prompt repl))))))
      (should (string-match-p "PRINTED" echoed))
      (should (string-match-p ":returned" echoed))
      ;; in the order the buffer shows them
      (should (< (string-match "PRINTED" echoed) (string-match ":returned" echoed))))))

(ert-deftest replique-test-what-nobody-waited-for-is-not-echoed ()
  "The echo area is for the answer to what was just sent.  What a repl
buffer was typed into is on screen already, and what arrives on its own
arrives while nobody is looking."
  (replique-test-with-repl repl
    (let ((echoed (replique-test-message
                    (replique-repl-send-code repl "(println \"UNASKED\")")
                    (replique-test-wait-for
                     (lambda () (replique-repl--at-prompt repl)))
                    (replique-test-settle))))
      (should (string-match-p "UNASKED" (replique-test-text repl)))
      (should-not echoed))))

(ert-deftest replique-test-output-that-arrives-on-its-own-is-tracked ()
  "A future prints long after the form that started it was answered.  The
result was reported where it was asked for; the printing has nowhere to
be reported to, so the mode line keeps a way to the buffer holding it."
  (replique-test-with-repl repl
    (let ((replique--unread nil)
          (global-mode-string nil)
          (buffer (replique-repl--buffer repl)))
      (replique-test-hide buffer)
      (replique-repl-send-code
       repl "(do (future (Thread/sleep 500) (println \"LATE\")) :started)"
       nil t)
      (should (replique-test-wait-for (lambda () (replique-repl--at-prompt repl))))
      ;; the result was reported where it was asked for, so nothing is owed
      ;; about it
      (should-not (memq buffer replique--unread))
      ;; at the end of a line: the code the buffer was sent is echoed into
      ;; it, and holds the string the future is about to print.  What the
      ;; future prints lands after the prompt, which is where output that
      ;; nobody is waiting for lands
      (should (replique-test-wait-for
               (lambda () (string-match-p "LATE$" (replique-test-text repl))) 10))
      (should (memq buffer replique--unread)))))

(ert-deftest replique-test-a-buffer-on-screen-is-a-buffer-nobody-is-told-about ()
  "Being shown is what the mode line is for; a buffer already shown needs
none of it, and one that is shown afterwards has been read."
  (let* ((replique--unread nil)
         (global-mode-string nil)
         (buffer (generate-new-buffer "*replique: tracked*"))
         (shown (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (replique-insert-output buffer "something\n")
          (should (memq buffer replique--unread))
          ;; named by what tells it apart, not by the decoration a buffer
          ;; list needs to tell it from a file
          (should (equal " tracked" (replique-unread-mode-line)))
          (set-window-buffer (selected-window) buffer)
          (replique-insert-output buffer "something else\n")
          (replique--unread-seen)
          (should-not replique--unread)
          (should (equal "" (replique-unread-mode-line))))
      (when (buffer-live-p shown)
        (set-window-buffer (selected-window) shown))
      (kill-buffer buffer))))

(ert-deftest replique-test-the-process-buffer-is-named-as-one ()
  "Two buffers of one process would otherwise be named the same thing."
  (let ((buffer (generate-new-buffer "*replique-process: a-project*")))
    (unwind-protect
        (should (equal "process: a-project" (replique--unread-name buffer)))
      (kill-buffer buffer))))

(ert-deftest replique-test-a-long-print-does-not-become-a-message ()
  "A form that printed a thousand lines is not something to put in the
echo area.  What is left out is not lost - it is in the buffer - so where
it was cut the buffer is named."
  (let* ((buffer (generate-new-buffer "*replique: long*"))
         (repl (replique-repl--make :buffer buffer :to-echo 0)))
    (unwind-protect
        (let ((lines (replique-repl--echo-shorten
                      repl (mapconcat #'number-to-string (number-sequence 1 100) "\n")))
              (long (replique-repl--echo-shorten repl (make-string 5000 ?x))))
          (should (equal replique-repl--echo-max-lines
                         (length (split-string lines "\n"))))
          (should (string-match-p "see \\*replique: long\\*" lines))
          (should (< (length long) 5000))
          (should (string-match-p "see \\*replique: long\\*" long))
          ;; and a short one is left alone
          (should (equal "nil" (replique-repl--echo-shorten repl "nil"))))
      (kill-buffer buffer))))

(ert-deftest replique-test-what-is-kept-for-the-echo-area-is-bounded ()
  "A form printing in a loop must not be accumulated in full for the sake
of the ten lines of it that will be shown."
  (let ((repl (replique-repl--make :to-echo 1)))
    (dotimes (_ 100)
      (replique-repl--echo-keep repl (make-string 1000 ?x)))
    (should (<= (length (replique-repl--echoed repl))
                (1+ replique-repl--echo-max-chars)))
    ;; kept past what is shown, which is what tells a cut one from a whole one
    (should (> (length (replique-repl--echoed repl)) replique-repl--echo-max-chars))))

(ert-deftest replique-test-nothing-is-kept-for-a-buffer-that-is-not-waiting ()
  (let ((repl (replique-repl--make :to-echo 0)))
    (replique-repl--echo-keep repl "printed")
    (should-not (replique-repl--echoed repl))))

;;; A process Emacs did not start

(ert-deftest replique-test-connecting-to-a-running-process ()
  "A process replique did not start reports its output as events rather
than through a pipe - which is the whole reason the protocol carries it."
  (let* ((workdir (file-name-as-directory (make-temp-file "replique-test" t)))
         (outside (replique-test-start
                   (format "{:directory \"%s\" :process-id \"outside\"}"
                           (directory-file-name workdir))))
         (process nil))
    (unwind-protect
        (progn
          (should (replique-test-wait-for
                   (lambda () (replique-process-descriptions workdir)) 120))
          (let ((known replique-processes))
            (replique-connect workdir)
            (should (replique-test-wait-for
                     (lambda () (seq-difference replique-processes known)) 30))
            (setq process (car (seq-difference replique-processes known))))
          (should (equal "outside" (replique-process--id process)))
          (let ((repl (replique-repl process)))
            (should (replique-test-wait-for (lambda () (replique-repl--ns repl))))
            (replique-test-eval
             repl "(do (.start (Thread. (fn [] (println \"TEED-OUT\")))) :started)")
            (should (replique-test-wait-for
                     (lambda ()
                       (with-current-buffer (replique-process-buffer process)
                         (string-match-p "^TEED-OUT$" (buffer-string))))
                     10))
            (replique-test-eval
             repl "(do (.start (Thread. (fn [] (throw (Exception. \"died\"))))) :started)")
            (should (replique-test-wait-for
                     (lambda ()
                       (with-current-buffer (replique-process-buffer process)
                         (string-match-p "Exception in thread" (buffer-string))))
                     10))))
      (when process (replique-kill-process process))
      (when (process-live-p outside) (delete-process outside))
      (delete-directory workdir t))))

(ert-deftest replique-test-a-process-emacs-did-not-start-can-be-asked-to-stop ()
  "There is nothing to signal - the process is no child of this Emacs, which
is what an Emacs that restarted has of every process it used to own.  Asking
is what is left, and it is what makes the process clean up after itself."
  (let* ((workdir (file-name-as-directory (make-temp-file "replique-test" t)))
         (outside (replique-test-start
                   (format "{:directory \"%s\" :process-id \"outside\"}"
                           (directory-file-name workdir))))
         (process nil))
    (unwind-protect
        (progn
          (should (replique-test-wait-for
                   (lambda () (replique-process-descriptions workdir)) 120))
          (let ((known replique-processes))
            (replique-connect workdir)
            (should (replique-test-wait-for
                     (lambda () (seq-difference replique-processes known)) 30))
            (setq process (car (seq-difference replique-processes known))))
          (should-not (replique-process--proc process))
          (should (string-match-p
                   "stopped" (replique-test-message (replique-kill-process process))))
          (should (replique-test-wait-for
                   (lambda () (not (process-live-p outside))) 30))
          ;; and it left through its shutdown hook, which is what tells a
          ;; process that was asked from one that was killed
          (should-not (replique-process-descriptions workdir)))
      (when (process-live-p outside) (delete-process outside))
      (delete-directory workdir t))))

(ert-deftest replique-test-a-process-that-will-not-stop-says-so ()
  "A process Emacs did not start that does not answer is a process this
command cannot stop.  What it must not do then is behave the way
`replique-disconnect\=' does under the name that promises the opposite: a
developer who is told the process stopped stops looking for it."
  (let* ((workdir (file-name-as-directory (make-temp-file "replique-test" t)))
         (outside (replique-test-start
                   (format "{:directory \"%s\" :process-id \"deaf\"}"
                           (directory-file-name workdir))))
         (process nil))
    (unwind-protect
        (progn
          (should (replique-test-wait-for
                   (lambda () (replique-process-descriptions workdir)) 120))
          (let ((known replique-processes))
            (replique-connect workdir)
            (should (replique-test-wait-for
                     (lambda () (seq-difference replique-processes known)) 30))
            (setq process (car (seq-difference replique-processes known))))
          (should-not (replique-process--proc process))
          (let ((said (cl-letf (((symbol-function 'replique-repl--ask-to-stop)
                                 (lambda (_process) nil)))
                        (replique-test-message (replique-kill-process process)))))
            (should (string-match-p "would not stop" said)))
          (should (process-live-p outside))
          (should (replique-process-descriptions workdir)))
      (when (process-live-p outside) (delete-process outside))
      (delete-directory workdir t))))

(ert-deftest replique-test-a-process-can-be-let-go-of ()
  "Letting go is not stopping: the process goes on running, and its port
file goes on saying where it is, so it can be connected to again."
  (let* ((workdir (file-name-as-directory (make-temp-file "replique-test" t)))
         (outside (replique-test-start
                   (format "{:directory \"%s\" :process-id \"outside\"}"
                           (directory-file-name workdir))))
         (process nil))
    (unwind-protect
        (progn
          (should (replique-test-wait-for
                   (lambda () (replique-process-descriptions workdir)) 120))
          (let ((known replique-processes))
            (replique-connect workdir)
            (should (replique-test-wait-for
                     (lambda () (seq-difference replique-processes known)) 30))
            (setq process (car (seq-difference replique-processes known))))
          (replique-disconnect process)
          (should-not (memq process replique-processes))
          ;; what the process would have done about it, had it been told
          (replique-test-settle)
          (should (process-live-p outside))
          (should (replique-process-descriptions workdir))
          (let ((known replique-processes))
            (replique-connect workdir)
            (should (replique-test-wait-for
                     (lambda () (seq-difference replique-processes known)) 30))
            (setq process (car (seq-difference replique-processes known)))))
      (when process (replique-kill-process process))
      (when (process-live-p outside) (delete-process outside))
      (delete-directory workdir t))))

;;; A port file that is wrong

(defun replique-test-free-port ()
  "Return a port nothing is listening on."
  (let* ((server (make-network-process :name "replique-test-free" :server t
                                       :host "127.0.0.1" :service t :noquery t))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun replique-test-write-port-file (directory info)
  "Write INFO in DIRECTORY the way a process writes its port file.

Returns the file."
  (let* ((dir (replique-processes-directory directory))
         (file (expand-file-name (format "%s.json" (plist-get info :process-id)) dir)))
    (make-directory dir t)
    (with-temp-file file (insert (json-serialize info) "\n"))
    file))

(ert-deftest replique-test-a-port-file-that-nothing-answers-is-dropped ()
  "A file naming a port of this machine that nothing listens on is about a
process that is gone.  Left there, it would go on being offered."
  (replique-test-with-project dir
    (let ((file (replique-test-write-port-file
                 dir (list :process-id "gone" :host "127.0.0.1"
                           :port (replique-test-free-port)
                           :directory (directory-file-name dir)
                           :pid 1 :started-at 1))))
      (should (file-exists-p file))
      (replique-connect dir)
      (should-not (file-exists-p file)))))

(ert-deftest replique-test-a-port-file-of-another-machine-is-kept ()
  "Nothing answering says the process is gone only when the file names this
machine: a host that is somewhere else can be unreachable for reasons of
its own.  What answers and says it is another process says the file is
wrong wherever that process runs."
  (replique-test-with-project dir
    (let ((file (replique-test-write-port-file
                 dir (list :process-id "elsewhere" :host "10.0.0.1" :port 1
                           :pid 1 :started-at 1))))
      (replique-process--reap file (replique-process--description file) 'unreachable)
      (should (file-exists-p file))
      (replique-process--reap file (replique-process--description file) 'mismatch)
      (should-not (file-exists-p file)))))

(ert-deftest replique-test-a-port-file-written-again-is-left-alone ()
  "A process that died and started again between the read and the connect
wrote a file of its own, and that one is about a process that is there."
  (replique-test-with-project dir
    (let* ((info (list :process-id "restarted" :host "127.0.0.1" :port 1
                       :pid 1 :started-at 1))
           (file (replique-test-write-port-file dir info)))
      (replique-test-write-port-file dir (plist-put (copy-sequence info) :pid 2))
      (replique-process--reap file info 'unreachable)
      (should (file-exists-p file)))))

(ert-deftest replique-test-a-port-file-whose-port-was-taken-over-is-dropped ()
  "The handshake names the process it expects, so a port that belongs to
another one refuses it - which is a file that is wrong, not a process that
is busy."
  (let ((process (replique-test-process)))
    (replique-test-with-project dir
      (let ((file (replique-test-write-port-file
                   dir (list :process-id "not-this-one"
                             :host (replique-process--host process)
                             :port (replique-process--port process)
                             :pid 1 :started-at 1))))
        (replique-connect dir)
        (should (replique-test-wait-for (lambda () (not (file-exists-p file))) 10))
        ;; and the process that refused it is untouched
        (should (replique-process-live-p process))))))

;;; Stopping a process

(ert-deftest replique-test-a-process-that-is-stopped-takes-its-port-file-with-it ()
  "A process is asked to stop rather than killed outright, so that the
shutdown hook deleting its port file runs.  A file left behind is a
process `replique-connect\=' goes on offering."
  (let ((project (replique-test-project)))
    (replique-test-with-project dir
      (let* ((replique-coordinates
              (format "{:local/root \"%s\"}" (directory-file-name project)))
             (known replique-processes)
             (process nil))
        (replique-start dir)
        (should (replique-test-wait-for
                 (lambda () (seq-difference replique-processes known)) 120))
        (setq process (car (seq-difference replique-processes known)))
        (let ((port-file (expand-file-name
                          (format ".replique/processes/%s.json"
                                  (replique-process--id process))
                          dir)))
          (should (file-exists-p port-file))
          ;; the wait is the command\='s, not the test\='s: what it says it did
          ;; is done when it returns
          (replique-kill-process process)
          (should-not (file-exists-p port-file)))))))

;;; Cleaning up

(defun replique-test-tear-down ()
  "Stop the process the tests shared."
  (when (replique-process-live-p replique-test-process)
    (replique-kill-process replique-test-process))
  (setq replique-test-process nil))

(add-hook 'kill-emacs-hook #'replique-test-tear-down)

(provide 'replique-test)

;;; replique-test.el ends here
