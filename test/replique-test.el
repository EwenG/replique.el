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
;; for the test that reads the package headers of replique.el
(require 'package)
(require 'find-func)
(require 'loaddefs-gen)
(require 'replique)

(defvar replique-test-process nil
  "The process shared by the tests.")

(defun replique-test-project ()
  "Return the Clojure project to start a process in, or skip the test.

An empty REPLIQUE_PROJECT is no project rather than the current
directory, which is what `expand-file-name' would make of it: the
makefile passes the variable through whether it was set or not, and the
tests that need a process would otherwise start one in the checkout they
are being run from - taking the name of the process the developer has
open on it."
  (let ((project (getenv "REPLIQUE_PROJECT")))
    (when (or (null project) (string-empty-p (string-trim project)))
      (ert-skip "REPLIQUE_PROJECT is not set"))
    (file-name-as-directory (expand-file-name project))))

(defmacro replique-test-with-clojure (text &rest body)
  "Run BODY in a Clojure buffer holding TEXT, with point at its beginning."
  (declare (indent 1))
  `(with-temp-buffer
     (replique-clojure-mode)
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defvar replique-test-sent nil
  "What was written on a repl connection, newest first, while captured.")

(defun replique-test-node-texts (nodes)
  "Return the text of NODES."
  (mapcar (lambda (node) (replique-parse-text node)) nodes))

(defun replique-test-forms (text &optional start end)
  "Return the text of the forms TEXT holds, as replique reads them.

START and END are where in it to look, the whole of it by default."
  (replique-test-with-clojure text
    (replique-test-node-texts
     (replique-eval--nodes (or start (point-min)) (or end (point-max))))))

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

;;; What a run that is killed leaves behind

(defconst replique-test-leftovers
  (expand-file-name "replique-test-leftovers" temporary-file-directory)
  "Where a run writes down what it has made, as it makes it.

A run that ends takes its processes and its temporary projects with it -
`replique-test-cleanup' does, on `kill-emacs-hook'.  A run that is killed
never runs that hook, and what it leaves is not idle: a process is started
under nohup so that it lives through the editor leaving, so it goes on
running and goes on holding its port file - which is what makes the next
process started in that project refuse to start.  A suite that hangs is
stopped with C-c, so this is the way a run ends often enough to matter.

What a dying Emacs cannot undo is left for the next run: every process and
every temporary project is written here the moment it exists, and
`replique-test-reap' reads the file before the suite starts.  Two suites
running at once would reap one another, which is the price of a name the
next run can find without being told it.")

(defun replique-test--note (entry)
  "Append ENTRY to `replique-test-leftovers'.

Written as it happens rather than at the end of the run: the run this is
for is the one whose end never comes."
  (with-temp-buffer
    (prin1 entry (current-buffer))
    (insert "\n")
    (write-region (point-min) (point-max) replique-test-leftovers t 'silent)))

(defun replique-test--noted ()
  "Return the entries `replique-test-leftovers' holds.

Read up to the first line that will not read: the file was written by a
run that was killed, and a last line cut off in the middle is what that
looks like.  What came before it still has to be undone."
  (when (file-exists-p replique-test-leftovers)
    (with-temp-buffer
      (insert-file-contents replique-test-leftovers)
      (goto-char (point-min))
      (let ((entries nil))
        (ignore-errors (while t (push (read (current-buffer)) entries)))
        (nreverse entries)))))

(defun replique-test--note-process (process)
  "Write down the operating system process PROCESS runs as, and return PROCESS."
  (when-let* ((pid (plist-get (replique-process--info process) :pid)))
    (replique-test--note (cons 'pid pid)))
  process)

(defun replique-test--note-running-in (directory)
  "Write down the processes DIRECTORY says are running in it.

For a start that was waited for and never arrived: the jvm may be coming
up all the same, and under nohup it would outlive the run that asked for
it - with nothing in the registry naming it, because it never connected.
Its port file is then the only thing that names it, and it is read here,
while it is about the start that just failed, rather than later, when it
may be about something else."
  (dolist (description (replique-process-descriptions directory))
    (when-let* ((pid (plist-get (cdr description) :pid)))
      (replique-test--note (cons 'pid pid)))))

(defun replique-test--ours-p (pid)
  "Return non-nil when PID is a replique process, and so one to signal.

A pid written down by a run that was killed can name anything by the time
the next run reads it - pids are handed out again - so nothing is
signalled on the strength of a number.  The command line has to say
replique.main, which is what the harness starts and what nothing else
here is.  Nil for a pid that is gone, which is also what makes this the
test for whether a process that was asked to stop has stopped."
  (when-let* ((args (cdr (assq 'args (process-attributes pid)))))
    (string-match-p "replique\\.main" args)))

(defun replique-test--kill-pid (pid)
  "Stop PID and wait for it to go.

Asked with TERM first, so that the process runs the shutdown hook that
deletes its port file - a port file left behind is most of what makes a
leftover process a problem - and KILLed when that goes unanswered."
  (when (replique-test--ours-p pid)
    (signal-process pid 'TERM)
    (let ((limit (+ (float-time) 10)))
      (while (and (replique-test--ours-p pid) (< (float-time) limit))
        (sleep-for 0.1)))
    (when (replique-test--ours-p pid)
      (signal-process pid 'KILL))))

(defun replique-test--temporary-p (directory)
  "Return non-nil when DIRECTORY is one of the projects the tests make.

A directory is deleted whole, so what is deleted has to be one the tests
wrote: the name `replique-test-with-project' asks for, under the
directory it asks for it in, and nothing else however it was noted."
  (string-prefix-p (expand-file-name "replique-test-project" temporary-file-directory)
                   (expand-file-name directory)))

(defun replique-test-reap ()
  "Stop and delete what a run that was killed left behind.

Run before the suite rather than after it: the run with something to undo
is not this one, and a leftover process holds the port file that would
stop this run starting anything in that project."
  (let ((entries (replique-test--noted)))
    ;; Processes before directories: a directory deleted under what is
    ;; still running in it takes the port file with it and leaves the
    ;; process, which is the half of the leak that matters
    (dolist (entry entries)
      (when (eq 'pid (car entry))
        (replique-test--kill-pid (cdr entry))))
    (dolist (entry entries)
      (when (and (eq 'dir (car entry)) (replique-test--temporary-p (cdr entry)))
        (ignore-errors (delete-directory (cdr entry) t)))))
  (when (file-exists-p replique-test-leftovers)
    (ignore-errors (delete-file replique-test-leftovers))))

;;; Starting a process

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
        ;; The launcher execs the jvm, so this is the jvm - and it is not
        ;; under nohup, which is what makes it Emacs's to lose and Emacs's
        ;; to take with it.  Noted all the same: what a run that is killed
        ;; leaves behind is not sorted by how it was started
        (let ((proc (make-process
                     :name "replique-test" :buffer nil
                     :command (list replique-clojure-program "-M" "-m" "replique.main" options)
                     :coding 'utf-8-unix :noquery t)))
          (replique-test--note (cons 'pid (process-id proc)))
          proc)
      (progn
        (replique-start project)
        (unless (replique-test-wait-for
                 (lambda () (seq-difference replique-processes known)) 120)
          ;; The jvm may be coming up all the same, and nothing in the
          ;; registry would name it
          (replique-test--note-running-in project)
          (error "The process did not start"))
        (replique-test--note-process
         (car (seq-difference replique-processes known)))))))

(defun replique-test-started-in (directory)
  "Start a process in DIRECTORY and return it once it has connected.

For a test that needs a project of its own - one written for the test, or
one reached by a name of the test's choosing.  The shared process is
`replique-test-process', and is what a test that only needs a process
should ask for."
  (let ((known replique-processes))
    (replique-start directory)
    (unless (replique-test-wait-for
             (lambda () (seq-difference replique-processes known)) 120)
      (replique-test--note-running-in directory)
      (error "The process did not start"))
    (replique-test--note-process
     (car (seq-difference replique-processes known)))))

(defun replique-test-cleanup ()
  "Stop every process the tests are still connected to.

Emacs no longer takes them with it: a process is started under nohup so
that it lives through the editor leaving, which a run of the tests has to
undo itself.  A process that survives holds the pipe it was started with,
and whatever reads that pipe waits for it - a test run piped into anything
would never end.

What Emacs is connected to is stopped through the command that stops one.
What is left after that is reaped by pid: a process whose control
connection dropped, and one that came up after a start had given up
waiting for it, are both gone from the registry and neither is gone from
the machine."
  (dolist (process (replique-processes-live))
    (replique-kill-process process))
  (replique-test-reap))

(add-hook 'kill-emacs-hook #'replique-test-cleanup)

;; Before anything is started rather than only after everything has been:
;; what is undone here belongs to a run that was killed, and the port files
;; it left are what would stop this run starting a process at all
(replique-test-reap)

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

`replique-repl' shows the buffer it opened, and what a buffer no window
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

(defvar replique-test--messages nil
  "Every message the code under test produced, newest first.")

(defmacro replique-test-messages (&rest body)
  "Run BODY and return every message it put in the echo area.

The last one is not enough to say that something was never said: what is
being asked is whether a line ever reached the echo area, and a message
after it is not an answer to that."
  (declare (indent 0))
  `(let ((replique-test--messages nil))
     (cl-letf (((symbol-function 'message)
                (lambda (format &rest args)
                  (push (apply #'format format args) replique-test--messages))))
       ,@body)
     replique-test--messages))

(defmacro replique-test-with-shown (buffer &rest body)
  "Run BODY with a window showing BUFFER, and put back what it showed."
  (declare (indent 1))
  `(let ((replique-test--shown (window-buffer (selected-window))))
     (unwind-protect
         (progn (set-window-buffer (selected-window) ,buffer)
                ,@body)
       (when (buffer-live-p replique-test--shown)
         (set-window-buffer (selected-window) replique-test--shown)))))

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

(ert-deftest replique-test-edn-a-list-that-starts-with-a-keyword-is-a-map ()
  "Which is what lets a message hold one: a keyword is what a key is here,
and a list of values that begins with one is not a thing this sends."
  (should (equal "{:name \"x\"}" (replique-edn-print (list :name "x"))))
  (should (equal "{:locals ({:name \"x\"} {:name \"y\"})}"
                 (replique-edn-map (list :locals (list (list :name "x")
                                                       (list :name "y"))))))
  (should (equal "(1 2)" (replique-edn-print (list 1 2)))))

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

(ert-deftest replique-test-a-start-that-lost-its-buffer-still-says-what-happened ()
  "The buffer holding what a process wrote can be killed while it is
starting.  There is then nowhere to point at, and pointing anyway is
`display-buffer' on a buffer that is not there - which is a signal
raised inside a process filter, in place of the report of what went
wrong."
  (let ((proc (make-process :name "replique-test-lost-buffer"
                            :command '("sleep" "5") :noquery t))
        (buffer (generate-new-buffer "*replique-test-startup*")))
    (unwind-protect
        (progn
          (process-put proc 'replique-buffer buffer)
          (process-put proc 'replique-state 'starting)
          (kill-buffer buffer)
          (should (equal "replique: the jvm would not start"
                         (replique-test-message
                           (replique-process--failed proc "the jvm would not start"))))
          (delete-process proc)
          (should (equal "replique: the process exited without starting"
                         (replique-test-message
                           (replique-process--spawn-sentinel proc "killed")))))
      (when (process-live-p proc) (delete-process proc)))))

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

(ert-deftest replique-test-your-aliases-are-yours-whatever-buffer-asks ()
  "A buffer visiting a file in one project carries that project's
directory local variables, and a start is for the project that was named.
A buffer local value would not be added to yours - it would be read
instead of them, and what you start every process with would go missing
from the one process it was set in the way of."
  (let ((replique-clojure-program "clojure")
        (replique-coordinates nil)
        (replique-aliases nil)
        (saved (default-value 'replique-user-aliases)))
    (unwind-protect
        (progn
          (setq-default replique-user-aliases '("my-tools"))
          (with-temp-buffer
            (setq-local replique-user-aliases '("another-projects"))
            (should (member "-M:my-tools"
                            (replique-process--command "/tmp/a-project/")))))
      (setq-default replique-user-aliases saved))))

(ert-deftest replique-test-what-replique-says-it-is-is-said-once ()
  "The version lives in the Version header of replique.el and nowhere
else: it is what package.el reads to know what it installed, and what
`replique-version\\=' says when somebody asks.

Written without its colon it is not a header - it is a comment that looks
like one, and what package.el makes of the file is a package with no
version at all.  Nothing else in the tree would notice, which is why this
is asked here."
  (let ((info (with-temp-buffer
                (insert-file-contents (find-library-name "replique"))
                (emacs-lisp-mode)
                (package-buffer-info))))
    (should (eq 'replique (package-desc-name info)))
    (should (equal (version-to-list replique--version)
                   (package-desc-version info)))))

(ert-deftest replique-test-what-builds-the-command-line-is-not-a-projects-to-set ()
  "The .dir-locals.el of a project being opened must not be able to offer
what the process runs with - not quietly, and not behind a prompt that
offers to remember the answer.  Which aliases a project needs is the
project's to say, and it is the one that is safe."
  (dolist (setting '(replique-clojure-program replique-coordinates
                                              replique-user-aliases
                                              replique-aliases-file))
    (should (risky-local-variable-p setting)))
  (should (safe-local-variable-p 'replique-aliases '("dev")))
  (should-not (safe-local-variable-p 'replique-aliases '(1 2))))

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
     (replique-test--note (cons 'dir ,name))
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
one.  `true' stands in for clojure: what is asserted is what the command did
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

;;; The name a path comes back under

(ert-deftest replique-test-two-names-for-one-directory-are-a-renaming ()
  "A process resolves the directory it runs in and an editor does not, so a
project opened through a symlink has two names.  What says so is that they
are different names for one place - a different name for a different place
is another process, and nothing to rename anything of."
  (replique-test-with-project dir
    (replique-test-with-project elsewhere
      (let ((link (concat (directory-file-name dir) "-link")))
        (unwind-protect
            (progn
              (make-symbolic-link (directory-file-name dir) link)
              (should-not (replique-process--renaming-between dir dir))
              (should-not (replique-process--renaming-between link elsewhere))
              (should-not (replique-process--renaming-between nil dir))
              (should-not (replique-process--renaming-between link nil))
              (let ((renaming (replique-process--renaming-between link dir)))
                (should renaming)
                (should (equal (file-name-as-directory dir) (car renaming)))
                (should (equal (file-name-as-directory link) (cdr renaming)))))
          (delete-file link))))))

(ert-deftest replique-test-only-what-is-under-the-directory-is-renamed ()
  "The prefix and nothing else.  A file the process names from somewhere
else - a jar under ~/.m2, a source beside the project - is named the one
way both of them have for it, and a directory whose name merely starts the
same is not under it at all."
  (let ((renaming (cons "/p/worktree/" "/p/repl/")))
    (should (equal "/p/repl/src/app.clj"
                   (replique-process--renamed-path "/p/worktree/src/app.clj" renaming)))
    (should (equal "/p/worktree-two/src/app.clj"
                   (replique-process--renamed-path "/p/worktree-two/src/app.clj" renaming)))
    (should (equal "/home/me/.m2/lib.jar"
                   (replique-process--renamed-path "/home/me/.m2/lib.jar" renaming)))
    (should (equal "/p/worktree/src/app.clj"
                   (replique-process--renamed-path "/p/worktree/src/app.clj" nil)))))

(ert-deftest replique-test-every-file-in-an-answer-is-renamed ()
  "Wherever one is.  An answer carries a file on its own, a file inside
what a name resolved to, and a list of them - and the next question is
asked with what came back, so one left behind would be asked about under a
name the editor never used."
  (let ((frame (replique-process--renamed
                '(:tag "reply" :op "usages"
                        :symbol (:name "thing" :ns "probe.core"
                                       :file "/p/worktree/src/probe/core.clj" :line 2)
                        :usages ((:file "/p/worktree/src/probe/core.clj" :line 3 :column 14)
                                 (:file "/p/elsewhere/src/probe/use.clj" :line 5)))
                (cons "/p/worktree/" "/p/repl/"))))
    (should (equal "/p/repl/src/probe/core.clj"
                   (plist-get (plist-get frame :symbol) :file)))
    (should (equal '("/p/repl/src/probe/core.clj" "/p/elsewhere/src/probe/use.clj")
                   (mapcar (lambda (use) (plist-get use :file))
                           (plist-get frame :usages))))
    ;; and the answer is otherwise the answer
    (should (equal "probe.core" (plist-get (plist-get frame :symbol) :ns)))
    (should (equal 14 (plist-get (car (plist-get frame :usages)) :column)))))

(ert-deftest replique-test-what-is-not-a-file-is-left-alone ()
  "What is renamed is settled by the key and never by the look of the text
under it.  A file is a `:file', which is what one is called throughout the
protocol, and plenty of text that is not one reads like a path all the
same: a docstring saying which file something reads, an arglist, an entry
naming a place inside an archive, the directory a process says it runs in.
Rewriting any of those would be rewriting what the process said."
  (let ((frame (replique-process--renamed
                '(:tag "reply" :directory "/p/worktree"
                        :symbol (:name "config"
                                       :file "/p/worktree/src/app.clj"
                                       :entry "clojure/string.clj"
                                       :doc "/p/worktree/etc/config.edn"
                                       :arglists ("[coll]" "/p/worktree/etc")))
                (cons "/p/worktree/" "/p/repl/"))))
    (should (equal "/p/worktree" (plist-get frame :directory)))
    (let ((found (plist-get frame :symbol)))
      (should (equal "/p/repl/src/app.clj" (plist-get found :file)))
      (should (equal "clojure/string.clj" (plist-get found :entry)))
      (should (equal "/p/worktree/etc/config.edn" (plist-get found :doc)))
      (should (equal '("[coll]" "/p/worktree/etc") (plist-get found :arglists))))))

(ert-deftest replique-test-a-process-the-editor-names-as-it-does-renames-nothing ()
  "Which is nearly every process there is, so it costs nothing: the frame
that arrived is the frame that is handed on, not a copy of it."
  (let ((process (replique-process--make :directory "/p/worktree" :renaming nil))
        (frame '(:tag "reply" :file "/p/worktree/src/app.clj")))
    (should (eq frame (replique-process--renamed-frame process frame)))))

(ert-deftest replique-test-a-process-is-found-under-either-name-of-its-directory ()
  "Which is what keeps a second one from being started where one is already
running: the guard is what the editor is asked for by name, and the name it
is asked about is whichever of the two somebody typed."
  (replique-test-with-project dir
    (let ((link (concat (directory-file-name dir) "-link")))
      (unwind-protect
          (progn
            (make-symbolic-link (directory-file-name dir) link)
            (replique-test-with-process-in link
              (should (replique-process-in link))
              (should (replique-process-in dir)))
            (replique-test-with-process-in dir
              (should (replique-process-in link))
              (should (replique-process-in dir))))
        (delete-file link)))))

(ert-deftest replique-test-every-way-of-asking-renames-what-comes-back ()
  "Both of them.  A command that waits for its answer and one that is told
later ask the same ops, and an answer renamed in one of them only would
send a name back under the other."
  (let ((process (replique-process--make
                  :directory "/p/repl"
                  :renaming (cons "/p/worktree/" "/p/repl/")))
        (answer '(:tag "reply" :file "/p/worktree/src/app.clj"))
        (heard nil))
    (cl-letf (((symbol-function 'replique-conn-live-p) (lambda (_conn) t))
              ((symbol-function 'replique-conn-request)
               (lambda (_conn _msg callback) (funcall callback answer) 1))
              ((symbol-function 'replique-conn-request-sync)
               (lambda (_conn _msg _timeout) answer)))
      (replique-process-request process '(:op :symbol)
                                (lambda (frame) (setq heard frame)))
      (should (equal "/p/repl/src/app.clj" (plist-get heard :file)))
      (should (equal "/p/repl/src/app.clj"
                     (plist-get (replique-process-request-sync process '(:op :symbol) 1)
                                :file))))))

(ert-deftest replique-test-what-comes-on-standard-error-is-not-the-startup-line ()
  "The clojure launcher writes on standard error while it resolves
dependencies - \"Downloading: ... from central\" - and `make-process\\=' mixes
the two streams into one filter unless it is told not to.  Read as the
startup line, the first of those is not a process announcing itself: every
start in a project whose dependencies are not all downloaded yet would be
given up on, while the jvm behind it came up, listened, and wrote the port
file that stops the next one.

Against a real process and a launcher that really does write there first,
because what is under test is which pipe a line arrives on."
  (let ((project (replique-test-project)))
    (replique-test-with-project dir
      (let ((script (expand-file-name "noisy-clojure" dir))
            (process nil))
        (with-temp-file (expand-file-name "deps.edn" dir)
          (insert "{:paths [\"src\"]}"))
        (with-temp-file script
          (insert "#!/bin/sh\n"
                  "echo 'Downloading: org/clojure/clojure/1.12.5/clojure-1.12.5.pom"
                  " from central' >&2\n"
                  "echo 'Downloading: org/clojure/clojure/1.12.5/clojure-1.12.5.jar"
                  " from central' >&2\n"
                  (format "exec %s \"$@\"\n"
                          (shell-quote-argument (executable-find "clojure")))))
        (set-file-modes script #o755)
        (let ((replique-clojure-program script)
              (replique-coordinates (format "{:local/root %S}" project)))
          (setq process (replique-test-started-in dir)))
        (should (replique-process-live-p process))
        (let ((text (with-current-buffer (replique-process-buffer process)
                      (buffer-string))))
          ;; What was written on it is still shown: the point is where it
          ;; goes, not that it goes nowhere
          (should (string-match-p "Downloading: org/clojure/clojure" text))
          ;; And shown as what it is, which is the rule an `err' event of a
          ;; connected process is shown under
          (should (eq 'replique-stderr
                      (get-text-property (string-match "Downloading:" text)
                                         'face text))))))))

(ert-deftest replique-test-a-process-reached-through-a-link-answers-under-the-link ()
  "The whole of it, against a real process - the only thing that resolves
the directory it was started in.  A project is reached through a symlink,
a file of it is loaded, and where the process says its definition is has to
be the file the editor opened: a jump that landed on the other side of the
link would take the buffer out of the worktree the link is pointing at."
  (let ((project (replique-test-project)))
    (replique-test-with-project dir
      (let* ((link (file-name-as-directory (concat (directory-file-name dir) "-link")))
             (source (expand-file-name "src/probe/core.clj" link))
             (process nil))
        (unwind-protect
            (progn
              (make-symbolic-link (directory-file-name dir) (directory-file-name link))
              (make-directory (expand-file-name "src/probe" dir) t)
              (with-temp-file (expand-file-name "deps.edn" dir)
                (insert "{:paths [\"src\"]}"))
              (with-temp-file (expand-file-name "src/probe/core.clj" dir)
                (insert "(ns probe.core)\n(defn thing [] 1)\n(defn one [] (thing))\n"))
              (let ((replique-coordinates (format "{:local/root %S}" project)))
                (setq process (replique-test-started-in link)))
              ;; The two names are really two, or the rest stands over nothing
              (should (replique-process--renaming process))
              (should (equal link (file-name-as-directory
                                   (replique-process--directory process))))
              (let ((repl (replique-repl process))
                    (asked (list :op :symbol :position :code
                                 :ns "probe.core" :text "thing")))
                (replique-test-hide (replique-repl--buffer repl))
                ;; Required rather than loaded from a buffer: a file loaded by
                ;; its path is answered under the path that was sent, and what
                ;; this is about is the process naming a file its own way -
                ;; which is what a require leaves behind, and what every file
                ;; a session has been running on is named by
                (replique-test-eval repl "(require (quote probe.core))")
                ;; The process really does answer something else, or the rest
                ;; of this would hold however the answer was handled
                (let ((raw (replique-conn-request-sync
                            (replique-process--control process) asked 10)))
                  (should (plist-get raw :symbol))
                  (should-not (equal source (plist-get (plist-get raw :symbol) :file))))
                (let ((buffer (find-file-noselect source)))
                  (unwind-protect
                      ;; Bound rather than set: it is where the commands of a
                      ;; Clojure buffer look for the repl to act on, and a test
                      ;; that left one behind would answer for the next test
                      (let ((replique-current-repl repl))
                        (with-current-buffer buffer
                          ;; Asked at a use of it, which is where a name is a
                          ;; name being used rather than one being given
                          (goto-char (point-min))
                          (search-forward "(thing)")
                          (forward-char -2)
                          (let ((found (replique-symbol--ask (replique-name-context)
                                                             "thing")))
                            (should found)
                            (should (equal source (plist-get found :file))))))
                    (kill-buffer buffer)))))
          (when process (replique-kill-process process))
          (when (file-symlink-p (directory-file-name link))
            (delete-file (directory-file-name link))))))))

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
  "That the repl is still at its prompt is the assertion that means
something.  What the buffer holds does not tell a newline made here from
one `comint-send-input' made on its way out: both leave the same text
behind, and the repl answers an unfinished form with nothing to show for
it either way.  What only one of them does is stop the repl reading
forms."
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(+ 1")
      (replique-repl-return))
    (replique-test-settle)
    (should (string-suffix-p "(+ 1\n" (replique-test-text repl)))
    (should (replique-repl--at-prompt repl))
    (with-current-buffer (replique-repl--buffer repl)
      (insert " 1)")
      (replique-repl-return))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^2$" (replique-test-text repl)))))))

(ert-deftest replique-test-a-form-closed-before-it-is-written-is-not-sent ()
  "A closing delimiter inserted with its opening one leaves the input
balanced from the first character typed.  Balance alone would send such a
form the moment its first line was done, and nothing could be typed over
two lines in a buffer where the closers arrive by themselves.  Where
point is says which was meant: a form still being written is written from
inside it."
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      ;; What `electric-pair-mode' leaves behind after "(+ 1 1"
      (insert "(+ 1 1)")
      (backward-char)
      (replique-repl-return))
    (replique-test-settle)
    (should (string-suffix-p "(+ 1 1\n)" (replique-test-text repl)))
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (replique-repl-return))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^2$" (replique-test-text repl)))))))

(ert-deftest replique-test-return-sends-from-the-middle-when-told-to ()
  "Where point is is a guess about what the developer is still writing,
and a guess is worth a way to overrule it."
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(+ 1 1)")
      (backward-char)
      (replique-repl-return t))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^2$" (replique-test-text repl)))))))

(ert-deftest replique-test-recalling-an-input-keeps-the-lines-already-typed ()
  "The input ring replaces what is at the prompt, and what is at the
prompt is the line being typed - not the form it is the third line of.
`comint-accumulate' is what says where that line began; a newline
inserted without it leaves the ring taking back to the process mark, and
recalling a previous input in the middle of a form throws away the lines
of it already written."
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(+ 2 2)")
      (replique-repl-return))
    (should (replique-test-wait-for
             (lambda () (string-match-p "^4$" (replique-test-text repl)))))
    (replique-test-settle)
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(+ 1")
      (replique-repl-return)
      (insert "   (* 3")
      (comint-previous-input 1)
      (should (string-suffix-p "(+ 1\n(+ 2 2)"
                               (buffer-substring-no-properties (point-min)
                                                               (point-max)))))))

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

(ert-deftest replique-test-a-password-prompt-is-not-comints-to-answer ()
  "What comint watches output for is a shell asking for a password, and
what it does about one is read the answer and send it to the process of
the buffer.  Here that process is the repl connection, so the answer
would go to the reader as code.  A repl printing \"Password: \" is a repl
printing something - what asks on a terminal is the jvm, on a standard
input the repl is not."
  (replique-test-with-repl repl
    (should-not (memq 'comint-watch-for-password-prompt
                      (buffer-local-value 'comint-output-filter-functions
                                          (replique-repl--buffer repl))))
    (let ((asked nil))
      (cl-letf (((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq asked t) "")))
        (replique-repl-send-code repl "(do (.write *out* \"Password: \") (.flush *out*))")
        (should (replique-test-wait-for
                 (lambda () (string-match-p "Password: " (replique-test-text repl)))))
        ;; What comint would do about it is done by a timer
        (replique-test-settle)
        (should-not asked)))))

(ert-deftest replique-test-what-is-typed-at-the-prompt-is-clojure ()
  "The buffer is given the syntax and the parse `replique-clojure-mode'
reads Clojure with, so that what is typed at the prompt is the code it is
rather than the text a comint buffer holds by default.  What comint puts
on the buffer itself survives being fontified by the parse: the prompt is
still a prompt to look at."
  (replique-test-with-repl repl
    (with-current-buffer (replique-repl--buffer repl)
      (goto-char (point-max))
      (insert "(defn foo [] ; )\n  :kw)")
      ;; The syntax table: a paren inside a comment closes nothing
      (let ((comment (save-excursion (search-backward "; )") (point))))
        (should (nth 4 (syntax-ppss (+ comment 2)))))
      (font-lock-mode 1)
      (font-lock-ensure)
      (let ((defn (save-excursion (search-backward "defn") (point))))
        (should (eq 'font-lock-keyword-face (get-text-property defn 'face))))
      (should (memq 'comint-highlight-prompt
                    (get-text-property (point-min) 'font-lock-face))))))

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
                    (setq replique-current-repl repl)
                    (replique-eval-region (point-min) (point-max)))
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
  (replique-test-with-clojure "(def a 1)\n\n(def b 2)\n"
    (let ((nodes (replique-eval--nodes (point-min) (point-max))))
      (should (equal '("(def a 1)" "(def b 2)") (replique-test-node-texts nodes)))
      (should (equal '(1 3)
                     (mapcar (lambda (n)
                               (line-number-at-pos (replique-parse-start n) t))
                             nodes))))))

(defun replique-test-ns-at (text needle)
  "Return the namespace TEXT names where NEEDLE is found in it."
  (replique-test-with-clojure text
    (search-forward needle)
    (goto-char (match-beginning 0))
    (replique-eval--ns-at (point))))

(ert-deftest replique-test-a-form-is-written-in-the-namespace-above-it ()
  "Which is what the code has to be read and evaluated in: a repl left in
another namespace would compile the definitions of one file into another."
  (should (equal "foo.bar"
                 (replique-test-ns-at "(ns foo.bar)\n(def a 1)\n" "(def a")))
  (should (equal "foo.bar"
                 (replique-test-ns-at "(ns ^{:author \"me\"} foo.bar)\n(def a 1)\n"
                                      "(def a")))
  ;; nothing above it names one
  (should (null (replique-test-ns-at "(def a 1)\n(ns foo.bar)\n" "(def a")))
  (should (null (replique-test-ns-at "(def a 1)\n" "(def a"))))

(ert-deftest replique-test-an-in-ns-applies-to-what-is-under-it ()
  "Evaluating code in another namespace is done by writing an in-ns above
it, which is how a scratch file reaches into one namespace and then
another.  Written the way it is written where clojure.core is not
referred, too."
  (should (equal "one"
                 (replique-test-ns-at "(ns foo.bar)\n(in-ns 'one)\n(def a 1)\n" "(def a")))
  (should (equal "foo.bar"
                 (replique-test-ns-at "(ns foo.bar)\n(def a 1)\n(in-ns 'one)\n" "(def a")))
  (should (equal "one"
                 (replique-test-ns-at "(clojure.core/in-ns 'one)\n(def a 1)\n" "(def a")))
  ;; the in-ns form itself is evaluated where it stands rather than in the
  ;; namespace it is about to enter
  (should (null (replique-test-ns-at "(in-ns 'one)\n(def a 1)\n" "(in-ns"))))

(ert-deftest replique-test-an-in-ns-inside-a-form-does-not-outlive-it ()
  "The (comment ...) case: a namespace entered inside a form is entered
for what is inside that form, and what follows the form is under whatever
was above it.  A level deeper than another overrides it, and only there."
  (let ((text (concat "(ns foo.bar)\n"
                      "(comment\n"
                      "  (in-ns 'scratch)\n"
                      "  (def inside 1))\n"
                      "(def after 2)\n")))
    (should (equal "scratch" (replique-test-ns-at text "(def inside")))
    (should (equal "foo.bar" (replique-test-ns-at text "(def after")))))

(ert-deftest replique-test-a-namespace-nobody-wrote-is-not-read ()
  "The buffer is read from the parse rather than from its text, so a
namespace named inside a string or behind a semicolon is a namespace
nobody asked to be in.  An argument that is computed rather than written
out names nothing that can be read either, and a qualified symbol is not
the name of a namespace at all."
  (should (null (replique-test-ns-at "\"(in-ns 'evil)\"\n(def a 1)\n" "(def a")))
  (should (null (replique-test-ns-at ";; (in-ns 'evil)\n(def a 1)\n" "(def a")))
  (should (null (replique-test-ns-at "(in-ns (symbol \"evil\"))\n(def a 1)\n" "(def a")))
  (should (null (replique-test-ns-at "(in-ns 'foo/bar)\n(def a 1)\n" "(def a")))
  ;; somebody else's in-ns, which does something else
  (should (null (replique-test-ns-at "(other.lib/in-ns 'evil)\n(def a 1)\n" "(def a"))))

(ert-deftest replique-test-evaluating-moves-the-repl-to-the-buffers-namespace ()
  "Code taken from a buffer is read and evaluated in the namespace that
buffer is in, and the repl stays there: going to the repl after having
evaluated something lands at a prompt of the namespace being worked in.

The definition is looked for in that namespace and not in the one the
repl was left in, which is the thing that would silently go wrong."
  (replique-test-with-repl repl
    (replique-test-with-clojure "(ns replique.test-target)\n(defn from-a-buffer [] :yes)\n"
      (setq replique-current-repl repl)
      (goto-char (point-max))
      (replique-eval-last-sexp))
    (replique-test-wait-for
     (lambda () (string-match-p "from-a-buffer" (replique-test-text repl))))
    (should (string-match-p "^:yes$"
                            (replique-test-eval
                             repl "(replique.test-target/from-a-buffer)")))
    ;; the prompt itself, which is what an editor reads the namespace off
    (should (equal "replique.test-target" (replique-repl--ns repl)))
    ;; and a namespace the process did not have is one it can work in:
    ;; in-ns alone makes a namespace where defn does not resolve
    (should (string-match-p "^:yes$" (replique-test-eval repl "(from-a-buffer)")))))

(ert-deftest replique-test-the-namespace-is-not-shown-as-something-somebody-wrote ()
  "The directive is protocol, like the source one: a transcript showing it
is a transcript of the wire."
  (replique-test-with-repl repl
    (replique-test-with-clojure "(ns replique.test-quiet)\n(def a 1)\n"
      (setq replique-current-repl repl)
      (goto-char (point-max))
      (replique-eval-last-sexp))
    (replique-test-wait-for
     (lambda () (string-match-p "replique.test-quiet/a" (replique-test-text repl))))
    (should-not (string-match-p "#replique/ns" (replique-test-text repl)))
    (should-not (string-match-p "#replique/src" (replique-test-text repl)))))

(ert-deftest replique-test-the-process-says-what-namespaces-it-has ()
  "What has been loaded, which is what a repl can be moved into: a
namespace that exists only as a file on the classpath is one nothing can
be evaluated in yet."
  (replique-test-with-repl repl
    (let ((namespaces (replique-namespaces (replique-repl-process repl))))
      (should (member "clojure.core" namespaces))
      (should (member "user" namespaces))
      (should-not (member "replique.test-not-loaded" namespaces))
      ;; and it is the process being asked rather than a list from anywhere
      ;; else: a namespace made now is in the next answer
      (replique-test-eval repl "(create-ns 'replique.test-just-made)")
      (should (member "replique.test-just-made"
                      (replique-namespaces (replique-repl-process repl)))))))

(ert-deftest replique-test-the-repl-can-be-moved-to-a-namespace ()
  "The command sends a directive rather than a form: there is no result
under it in the transcript, and nothing shown as having been typed.  What
says it worked is the prompt, which is what an editor reads the namespace
off anyway."
  (replique-test-with-repl repl
    (setq replique-current-repl repl)
    ;; one the process has: clojure.set is not loaded in a fresh one, and a
    ;; namespace that exists only as a file on the classpath is a different
    ;; case - see the tests of the directive itself
    (replique-test-eval repl "(require 'clojure.set)")
    (let ((before (replique-test-text repl)))
      (replique-in-ns "clojure.set")
      (replique-test-wait-for
       (lambda () (equal "clojure.set" (replique-repl--ns repl))))
      (should (equal "clojure.set" (replique-repl--ns repl)))
      ;; the prompt that was standing said user, and nothing consumed it, so
      ;; the new one goes on a line of its own under it rather than beside it
      (should (equal "\nclojure.set=> "
                     (substring (replique-test-text repl) (length before)))))
    ;; and the repl really is in it: what that namespace holds resolves
    ;; without being named
    (should (string-match-p "^:in-there$"
                            (replique-test-eval
                             repl "(if (resolve 'union) :in-there :not)")))
    ;; a namespace the process does not have is one it makes, with
    ;; clojure.core referred into it - see `enter-ns!' in replique.repl
    (replique-in-ns "replique.test-brand-new")
    (replique-test-wait-for
     (lambda () (equal "replique.test-brand-new" (replique-repl--ns repl))))
    (should (string-match-p "^2$" (replique-test-eval repl "(inc 1)")))))

(ert-deftest replique-test-the-namespace-offered-is-the-one-the-buffer-is-in ()
  "Moving the repl to where the code being worked on lives is what the
command is nearly always for, so that is what pressing RET does.  In a
repl buffer there is no such namespace and nothing is offered.

Asked of the whole buffer rather than of what a narrowing left of it: a
parse of what is reachable does not hold an ns form that is not, and a
buffer narrowed to one function is still a buffer of that namespace."
  (replique-test-with-repl repl
    (setq replique-current-repl repl)
    (let ((asked nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest args)
                   (setq asked (list (nth 4 args) collection))
                   "clojure.set")))
        (replique-test-with-clojure "(ns replique.test-offered)\n(def a 1)\n"
          (setq replique-current-repl repl)
          (goto-char (point-max))
          (call-interactively #'replique-in-ns))
        (should (equal "replique.test-offered" (car asked)))
        (should (member "clojure.core" (nth 1 asked)))
        ;; and the same buffer narrowed to below its ns form
        (setq asked nil)
        (replique-test-with-clojure "(ns replique.test-offered)\n(def a 1)\n"
          (setq replique-current-repl repl)
          (goto-char (point-min))
          (search-forward "(def a")
          (narrow-to-region (match-beginning 0) (point-max))
          (call-interactively #'replique-in-ns))
        (should (equal "replique.test-offered" (car asked)))
        ;; a repl buffer is not a buffer with a namespace written in it
        (setq asked nil)
        (with-current-buffer (replique-repl--buffer repl)
          (call-interactively #'replique-in-ns))
        (should (null (car asked)))))))

(ert-deftest replique-test-what-is-not-a-namespace-name-is-not-sent ()
  "What is typed becomes a line of the repl's input, and the reader takes
the first token of it as the directive and reads the rest as a form.  So
\"foo bar\" would move the repl to foo and evaluate bar, and neither is
what was asked for.  The process cannot report that: it only ever sees
what it managed to read."
  (replique-test-with-repl repl
    (setq replique-current-repl repl)
    (let ((before (replique-test-text repl)))
      (dolist (typed '("foo bar" "foo)" "#foo" "foo/bar" "^foo" "a;b" "  "))
        (should-error (replique-in-ns typed) :type 'user-error))
      (replique-test-settle)
      ;; nothing of it reached the repl
      (should (equal before (replique-test-text repl)))
      (should (equal "user" (replique-repl--ns repl))))
    ;; and a name that is one is sent
    (replique-in-ns "replique.test-well-formed")
    (should (replique-test-wait-for
             (lambda () (equal "replique.test-well-formed"
                               (replique-repl--ns repl)))))))

(ert-deftest replique-test-a-region-reaching-over-an-in-ns-lands-in-both ()
  "One directive is sent, before the first form, where a source directive
is needed per form: this one moves the repl and leaves it moved.  What
would move it again further down is an ns or an in-ns form between two of
the forms being sent - and such a form is one of them, since a region
takes in every form it reaches over and drops only comments.  So the rest
of the region says where it is going by being evaluated, and a directive
per form would be sending what the code already says.

The region starts under the ns form rather than at it, which is what
makes the directive the only thing that can put the first def where it
belongs: the repl is in user, and nothing evaluated here moves it there."
  (replique-test-with-repl repl
    (setq replique-test-sent nil)
    (should (equal "user" (replique-repl--ns repl)))
    (replique-test-with-clojure
        (concat "(ns replique.test-region-first)\n"
                "(def a :first)\n"
                "(in-ns 'replique.test-region-second)\n"
                "(def b :second)\n")
      (setq replique-current-repl repl)
      (goto-char (point-min))
      (search-forward "(def a")
      (cl-letf* ((send (symbol-function 'replique-conn-send-code))
                 ((symbol-function 'replique-conn-send-code)
                  (lambda (conn code)
                    (push code replique-test-sent)
                    (funcall send conn code))))
        (replique-eval-region (match-beginning 0) (point-max))))
    (should (replique-test-wait-for
             (lambda () (equal "replique.test-region-second" (replique-repl--ns repl)))))
    ;; each def where the code says, and nothing of the second half left
    ;; behind in the first
    (should (string-match-p
             "\"replique.test-region-first\""
             (replique-test-eval
              repl (concat "(clojure.core/str (:ns (clojure.core/meta "
                           "(clojure.core/resolve 'replique.test-region-first/a))))"))))
    (should (string-match-p
             "\"replique.test-region-second\""
             (replique-test-eval
              repl (concat "(clojure.core/str (:ns (clojure.core/meta "
                           "(clojure.core/resolve 'replique.test-region-second/b))))"))))
    (should (string-match-p
             "^false$"
             (replique-test-eval
              repl (concat "(clojure.core/some? (clojure.core/resolve "
                           "'replique.test-region-first/b))"))))
    ;; and only the one went out, whatever the region held
    (should (equal 1 (1- (length (split-string (car replique-test-sent)
                                               "#replique/ns")))))))

(ert-deftest replique-test-a-clojure-buffer-has-the-commands-in-it ()
  "The keys are bound in a Clojure file without anything being turned on
by hand: the mode replique opens one in is what turns them on."
  (replique-test-with-clojure "(def a 1)\n"
    (should replique-mode)
    (should (eq #'replique-eval-defun (key-binding (kbd "C-M-x"))))
    (should (eq #'replique-load-file (key-binding (kbd "C-c C-l"))))
    (should (eq #'replique-remove-var (key-binding (kbd "C-c C-u"))))))

(defmacro replique-test--in-repl-mode (&rest body)
  "Run BODY in a buffer in `replique-repl-mode', with no repl behind it.
Which is enough to ask what a key is bound to there, and asks it of the
mode rather than of a process."
  (declare (indent 0))
  `(with-temp-buffer
     (replique-repl-mode)
     ,@body))

(ert-deftest replique-test-a-repl-buffer-shows-the-process-output ()
  "`C-c C-o' is the same command at a prompt as in a file.  comint has
`comint-delete-output' there, which writes \"*** output flushed ***\" into
the transcript."
  (replique-test--in-repl-mode
    (should (eq #'replique-show-process-output (key-binding (kbd "C-c C-o"))))))

(ert-deftest replique-test-a-repl-buffer-does-not-signal-a-subjob ()
  "There is no subjob: the process of a repl buffer is a socket.  Stopping
one would stop Emacs reading it - a repl that looks hung - and quitting one
asks for a signal a connection cannot carry.  So the keys say they are
undefined rather than falling through to comint."
  (replique-test--in-repl-mode
    (should (eq #'undefined (key-binding (kbd "C-c C-z"))))
    (should (eq #'undefined (key-binding (kbd "C-c C-\\"))))))

(ert-deftest replique-test-a-repl-buffer-still-interrupts-and-quits ()
  "What those keys are reached for is on the keys that do it here."
  (replique-test--in-repl-mode
    (should (eq #'replique-interrupt (key-binding (kbd "C-c C-c"))))
    (should (eq #'replique-quit-repl (key-binding (kbd "C-c C-q"))))))

(ert-deftest replique-test-a-repl-buffer-completes-and-answers ()
  "Completion, eldoc and xref are turned on by the mode, so a repl opened
from an autoload - without `replique.el' having been loaded by anything -
is one they answer in.  Without this a repl completed filenames, which is
what a comint buffer does when nobody else offers."
  (replique-test--in-repl-mode
    (should (memq #'replique-completion-at-point completion-at-point-functions))
    (should (memq #'replique-symbol-eldoc eldoc-documentation-functions))
    (should (memq #'replique-symbol-xref-backend xref-backend-functions))))

(ert-deftest replique-test-the-repl-hooks-are-autoloaded ()
  "A repl opened by `replique-start\\=' - autoloaded out of another file - is
set up although nothing has loaded `replique.el\\='.  What tells package.el
to do that is the autoloads it generates, so they are generated here and
asked, rather than the source being read for a cookie."
  (let* ((source (file-name-directory (locate-library "replique")))
         ;; Into a directory of its own: `loaddefs-generate' writes the file
         ;; itself, and leaves one that is already there alone
         (dir (make-temp-file "replique-autoloads" t))
         (out (expand-file-name "replique-autoloads.el" dir)))
    (unwind-protect
        (let ((inhibit-message t))
          (loaddefs-generate source out)
          (with-temp-buffer
            (insert-file-contents out)
            (goto-char (point-min))
            (should (search-forward
                     "(add-hook 'replique-repl-mode-hook #'replique-completion-install)"
                     nil t))
            (goto-char (point-min))
            (should (search-forward
                     "(add-hook 'replique-repl-mode-hook #'replique-symbol-install)"
                     nil t))))
      (delete-directory dir t))))

(ert-deftest replique-test-a-buffer-that-is-not-ours-is-not-evaluated ()
  "Evaluating reads the buffer with the grammar it is highlighted with, so
a buffer that is not read that way is one to say so about rather than to
guess at."
  (with-temp-buffer
    (fundamental-mode)
    (insert "(def a 1)\n")
    (should-error (replique-eval-region (point-min) (point-max))
                  :type 'user-error)))

(ert-deftest replique-test-a-comment-between-two-forms-is-not-a-form ()
  (should (equal '("(def a 1)")
                 (replique-test-forms ";; a comment\n(def a 1)\n;; another\n"))))

(ert-deftest replique-test-a-comment-is-never-what-gets-sent ()
  "Not from a region, not from point, not from a region holding one.

The directive replique writes applies to the next form, and a comment is
not one: the reader answers it with a prompt and the directive stays
pending, so what is read next - a form typed at the prompt - is recorded
in a file it was never in."
  ;; What is signalled and not only that something is: there is no repl
  ;; here, so a comment that got as far as being sent would fail too - and
  ;; it would fail saying there is nowhere to send it
  ;; point on it
  (replique-test-with-clojure "(def a 1)\n;; a note\n"
    (search-forward "a note")
    (should (equal '(user-error "No form at point")
                   (should-error (replique-eval-defun) :type 'user-error))))
  ;; and the blank line under it, where the fallback for point just after
  ;; a form looks
  (replique-test-with-clojure "(def a 1)\n;; a note\n\n"
    (goto-char (point-max))
    (should (equal '(user-error "No form at point")
                   (should-error (replique-eval-defun) :type 'user-error))))
  ;; a region that is exactly one, which is the whole of what it covers
  (replique-test-with-clojure ";; only a note\n"
    (should (equal '(user-error "Nothing to evaluate")
                   (should-error (replique-eval-region (point-min) (1- (point-max)))
                                 :type 'user-error)))))

(ert-deftest replique-test-a-comment-is-skipped-to-the-form-behind-it ()
  "Which is what `eval-last-sexp' does in Emacs Lisp, and what makes
C-x C-e work at the end of a file whose last line is a note."
  (replique-test-with-clojure "(def a 1)\n;; a note\n;; and another\n"
    (goto-char (point-max))
    (should (equal "(def a 1)"
                   (replique-parse-text (replique-eval--before (point)))))))

(ert-deftest replique-test-a-discarded-form-is-one-form ()
  "What #_ discards is part of the form it discards, not a form before it.

A directive written between the two is what the discard eats, and the
form that was commented out is then the one evaluated."
  (should (equal '("(def a 1)" "#_(def b 2)")
                 (replique-test-forms "(def a 1)\n#_(def b 2)\n")))
  (should (equal '("#_#_(x)(y)" "(z)")
                 (replique-test-forms "#_#_(x)(y)\n(z)\n")))
  (should (equal '("#_ ;; why\n(z)")
                 (replique-test-forms "#_ ;; why\n(z)\n"))))

(ert-deftest replique-test-nothing-empty-is-sent ()
  "A trailing #_ discards a form that is not there, and what it discards
is what would be sent.  Sending nothing writes a directive with nothing
after it, which is a directive nothing consumes - the same damage a
comment does, by another road.

It is refused for having no form rather than for having an empty one.
A grammar answers a construct left unfinished with a node of no width
standing where the form would have been, and reading the text says so
only once the node is in hand; a reader that answers with nothing says
so where the form was looked for."
  (replique-test-with-clojure "(def a 1)\n#_\n"
    (goto-char (point-max))
    (should (equal '(user-error "No form at point")
                   (should-error (replique-eval-defun) :type 'user-error))))
  (replique-test-with-clojure "(def a 1)\n#_ ;; why\n"
    (goto-char (point-max))
    (should (equal '(user-error "No form at point")
                   (should-error (replique-eval-defun) :type 'user-error)))))

(ert-deftest replique-test-metadata-is-part-of-the-form-it-is-on ()
  "A directive between the metadata and the definition is what the
metadata ends up on, and the definition is left without it."
  (should (equal '("^{:m 1}\n(def c 3)")
                 (replique-test-forms "^{:m 1}\n(def c 3)\n")))
  (should (equal '("^:private (def c 3)")
                 (replique-test-forms "^:private (def c 3)\n"))))

(ert-deftest replique-test-a-reader-macro-is-not-a-form-of-its-own ()
  "The ones sexp motion reads as two, and the ones it gets right, in one
list: what is being pinned is that every one of them is a single form."
  (let ((forms '("#{1 2}" "#(inc %)" "#?(:clj 1)" "#?@(:clj [1])" "#\"re\""
                 "~@(a)" "'(1)" "`(1)" "#=(+ 1 1)" "#^String x" "#'foo" "@(f)")))
    (should (equal forms (replique-test-forms (string-join forms "\n"))))))

(ert-deftest replique-test-what-cannot-be-read-is-sent-as-it-is ()
  "The reader says what is wrong with an unfinished form better than
anything here could, so it is what gets to say it."
  (should (equal '("(def a 1)" "(def b\n")
                 (replique-test-forms "(def a 1)\n(def b\n"))))

(ert-deftest replique-test-a-region-inside-a-form-is-the-forms-inside-it ()
  "Selecting expressions in the body of a function evaluates those
expressions.  Nothing at the top level starts in that region, and
answering with the whole function would be answering another question."
  (replique-test-with-clojure "(defn f []\n  (a 1)\n  (b 2)\n  (c 3))\n"
    (let* ((start (progn (search-forward "(a 1)") (match-beginning 0)))
           (after-a (point))
           (after-b (progn (search-forward "(b 2)") (point))))
      (should (equal '("(a 1)" "(b 2)")
                     (replique-test-node-texts (replique-eval--nodes start after-b))))
      (should (equal '("(a 1)")
                     (replique-test-node-texts
                      (replique-eval--nodes start after-a)))))))

(ert-deftest replique-test-a-form-half-selected-is-evaluated-whole ()
  "Half a form is a read error, not an evaluation."
  (should (equal '("(def a 1)")
                 (replique-test-forms "(def a 1)\n(def b 2)\n" 1 5))))

(ert-deftest replique-test-the-form-point-is-on ()
  "What a point command acts on: the whole of the top level form, its
metadata included, and point just after a form counts as being on it -
which is where typing one leaves point."
  (replique-test-with-clojure "(def a 1)\n^{:m 1}\n(def c 3)\n"
    (search-forward "def c")
    (should (equal "^{:m 1}\n(def c 3)"
                   (replique-parse-text (replique-eval--covering (point)))))
    (goto-char (point-min))
    (end-of-line)
    (should (equal "(def a 1)"
                   (replique-parse-text (replique-eval--covering (1- (point))))))))

(ert-deftest replique-test-the-form-before-point ()
  "The largest form ending there: point after the last paren of (a (b))
is at the end of both, and the one just finished is the outer one."
  (replique-test-with-clojure "(a (b))"
    (should (equal "(a (b))"
                   (replique-parse-text (replique-eval--ending-at (point-max))))))
  (replique-test-with-clojure "(x)\n^{:m 1} (def c 3)"
    (should (equal "^{:m 1} (def c 3)"
                   (replique-parse-text (replique-eval--ending-at (point-max)))))))

(ert-deftest replique-test-a-discard-under-point-is-evaluated ()
  "The way back from having commented a form out: putting point on it and
asking for it means the form, not the discarding of it.  A region does
not descend that way - what was commented out in a region was selected as
commented out."
  (replique-test-with-clojure "#_(def b 2)\n"
    (search-forward "def b")
    (should (equal "(def b 2)"
                   (replique-parse-text
                    (replique-eval--discarded (replique-eval--covering (point)))))))
  ;; and the last of a stacked one, there being no better answer
  (replique-test-with-clojure "#_#_(x)(y)\n"
    (should (equal "(y)"
                   (replique-parse-text
                    (replique-eval--discarded (replique-eval--covering (point))))))))

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
                    (setq replique-current-repl repl)
                    (replique-eval-region (point-min) (point-max)))
                (kill-buffer buffer)))
            (should (replique-test-wait-for
                     (lambda ()
                       (string-match-p "#'user/marker" (replique-test-text repl)))))
            (let ((text (replique-test-text repl)))
              (should (string-match-p "(def marker :here)" text))
              (should-not (string-match-p "replique/src" text))))
        (delete-file file)))))

(ert-deftest replique-test-a-comment-leaves-no-directive-behind ()
  "The whole of it, against a real reader.

A directive is consumed by the next form and by nothing else, and a
comment is answered with a prompt rather than with a form - so a client
that sends one leaves the directive pending, and the form read after it
is recorded in the file and at the line of the comment.  What is asserted
is the form typed at the prompt afterwards, which is where the damage
would show."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique-test-comment.clj" temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "(def a 1)\n\n\n\n\n\n;; a note on line 7\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (setq replique-current-repl repl)
                    (goto-char (point-max))
                    (should (equal '(user-error "No form at point")
                                   (should-error (replique-eval-defun)
                                                 :type 'user-error))))
                (kill-buffer buffer)))
            (replique-test-eval repl "(defn typed-at-the-prompt [])")
            (should (string-match-p
                     "^1$" (replique-test-eval
                            repl "(:line (meta #'typed-at-the-prompt))"))))
        (delete-file file)))))

(ert-deftest replique-test-a-trailing-discard-leaves-no-directive-behind ()
  "The other road to a directive nothing consumes, against a real reader.

Quieter than the comment: a comment is answered with a prompt, and this
is answered with nothing at all - the repl goes on waiting, and what says
something happened is the line the next form is recorded at."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique-test-discard-alone.clj"
                                  temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "(def a 1)\n\n\n\n\n\n#_\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (setq replique-current-repl repl)
                    (goto-char (point-max))
                    (should (equal '(user-error "No form at point")
                                   (should-error (replique-eval-defun)
                                                 :type 'user-error))))
                (kill-buffer buffer)))
            (replique-test-eval repl "(defn after-the-discard [])")
            (should (string-match-p
                     "^1$" (replique-test-eval
                            repl "(:line (meta #'after-the-discard))"))))
        (delete-file file)))))

(ert-deftest replique-test-a-discarded-form-is-not-evaluated ()
  "The whole of it, against a real reader: a directive written between #_
and the form it discards is the form the discard eats, and the one that
was commented out is then read and evaluated."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique-test-discard.clj" temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "(def evidence :untouched)\n"
                      "#_(def evidence :the-discard-ran)\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (setq replique-current-repl repl)
                    (replique-eval-region (point-min) (point-max)))
                (kill-buffer buffer)))
            (should (replique-test-wait-for
                     (lambda ()
                       (string-match-p "#'user/evidence" (replique-test-text repl)))))
            (should (string-match-p "^:untouched$"
                                    (replique-test-eval repl "evidence"))))
        (delete-file file)))))

(ert-deftest replique-test-multibyte-survives-the-round-trip ()
  "Everything is utf-8, in both directions.  The assertion is made on text
the form produced rather than on text it was given, so a round trip that
lost something cannot pass by echoing the question back."
  (replique-test-with-repl repl
    (let ((added (replique-test-eval
                  repl "(str (.toUpperCase \"café\") (apply str (repeat 2 \"🎉\")))")))
      (should (string-match-p "\"CAFÉ🎉🎉\"" added)))))


;;; Loading a file

(ert-deftest replique-test-a-buffer-of-a-file-is-that-file ()
  "What goes out about a buffer is the file it holds."
  (let ((file (expand-file-name "replique-test-what.clj" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(ns replique.test-what)\n"))
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (should (equal (list :file file) (replique-buffer-file))))
              (kill-buffer buffer))))
      (delete-file file))))

(ert-deftest replique-test-a-buffer-of-an-archive-is-a-file-and-an-entry ()
  "There is no path to a file inside an archive, so the name such a buffer
visits is not one - it is the two halves written together, and nothing but
the buffer that made it knows how to take it apart.  So the halves are kept
on the buffer, and it is those that go out."
  (with-temp-buffer
    (setq-local replique-archive-file "/home/me/.m2/clojure-1.12.5.jar")
    (setq-local replique-archive-entry "clojure/string.clj")
    (should (equal (list :file "/home/me/.m2/clojure-1.12.5.jar"
                         :entry "clojure/string.clj")
                   (replique-buffer-file)))
    ;; and through the mode the buffer is given, which is given to it after it
    ;; has been filled - turning a mode on kills the local variables of a
    ;; buffer, and what these say is not about the mode
    (replique-clojure-mode)
    (should (equal (list :file "/home/me/.m2/clojure-1.12.5.jar"
                         :entry "clojure/string.clj")
                   (replique-buffer-file)))))

(ert-deftest replique-test-a-buffer-of-no-file-is-nothing-to-point-at ()
  (with-temp-buffer
    (should-not (replique-buffer-file))))

(ert-deftest replique-test-the-load-directive-names-what-to-load ()
  "A file, and the entry beside it where that file is an archive - which is
how a file inside a jar is written throughout the protocol, so what came back
from asking where a definition was written is what goes out to load it."
  (should (equal "#replique/load {:file \"/a/b.clj\"}"
                 (replique-load-directive '(:file "/a/b.clj"))))
  (should (equal "#replique/load {:file \"/a/b.jar\" :entry \"c/d.clj\"}"
                 (replique-load-directive '(:file "/a/b.jar" :entry "c/d.clj")))))

(ert-deftest replique-test-loading-a-file-reads-it-as-one-unit ()
  "Which is not the same as evaluating its forms: the ns form runs first, so
what the file defines lands in the namespace the file names, without the
client having said anything about which namespace that is."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique_test_loaded.clj" temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "(ns replique.test-loaded)\n"
                      "(println \"loading\")\n"
                      "(defn answer [] :loaded)\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (setq replique-current-repl repl)
                    (replique-load-file))
                (kill-buffer buffer)))
            (should (replique-test-wait-for
                     (lambda () (string-match-p "loading" (replique-test-text repl)))))
            ;; the repl never moved: it is still in user, and the definition is
            ;; in the namespace the file named
            (should (equal "user" (replique-repl--ns repl)))
            (should (string-match-p
                     "^:loaded$"
                     (replique-test-eval repl "(replique.test-loaded/answer)"))))
        (delete-file file)))))

(ert-deftest replique-test-what-was-asked-for-is-shown-in-the-repl ()
  "A source directive is protocol and is kept out of the transcript, because
the form written under it is what there is to show.  A load directive is the
whole of what was asked for, and the output and the result about to arrive
would otherwise stand under nothing."
  (replique-test-with-repl repl
    (let ((file (expand-file-name "replique_test_shown.clj" temporary-file-directory)))
      (unwind-protect
          (progn
            (with-temp-file file (insert "(ns replique.test-shown)\n"))
            (let ((buffer (find-file-noselect file)))
              (unwind-protect
                  (with-current-buffer buffer
                    (setq replique-current-repl repl)
                    (replique-load-file))
                (kill-buffer buffer)))
            (should (replique-test-wait-for
                     (lambda () (string-match-p (regexp-quote "#replique/load")
                                                (replique-test-text repl)))))
            (should (string-match-p (regexp-quote file) (replique-test-text repl))))
        (delete-file file)))))

(defmacro replique-test--loading (&rest body)
  "Run BODY with the repl stubbed, binding `asked' and `sent'.

What is being checked is what happens to the buffer before the directive
goes out, so the repl is not needed and the directive is captured rather
than written."
  (declare (indent 0))
  `(let ((asked nil) (sent nil))
     (ignore asked sent)
     (cl-letf (((symbol-function 'y-or-n-p)
                (lambda (prompt) (setq asked prompt) replique-test--answer))
               ((symbol-function 'replique-repl-ensure-here) (lambda () 'a-repl))
               ((symbol-function 'replique-repl-send-code)
                (lambda (_repl code &rest _) (setq sent code))))
       ,@body)))

(defvar replique-test--answer t
  "What the stubbed `y-or-n-p' says while a load is being tested.")

(ert-deftest replique-test-a-buffer-is-saved-before-it-is-loaded ()
  "The process reads the file off the disk, so what a buffer with unsaved
changes would load is not what is on the screen.

Asked with `comint-check-source', which is the question Emacs already has
for this: the modes that run a language in a buffer have been asking it
since long before Clojure."
  (let ((file (expand-file-name "replique-test-unsaved.clj" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(ns replique.test-unsaved)\n"))
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (goto-char (point-max))
                  (insert "(def a 1)\n")
                  (should (buffer-modified-p))
                  (let ((replique-test--answer t))
                    (replique-test--loading
                      (replique-load-file)
                      (should asked)
                      (should-not (buffer-modified-p))
                      (should (string-match-p
                               "(def a 1)"
                               (with-temp-buffer (insert-file-contents file)
                                                 (buffer-string))))
                      (should (string-match-p (regexp-quote file) sent)))))
              (set-buffer-modified-p nil)
              (kill-buffer buffer))))
      (delete-file file))))

(ert-deftest replique-test-declining-loads-what-was-last-saved ()
  "Which is a real thing to want: reverting an experiment is loading what was
last saved.  Declining leaves the buffer as it is and loads all the same -
saying no to the question is not saying no to the command."
  (let ((file (expand-file-name "replique-test-ondisk.clj" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(ns replique.test-ondisk)\n"))
          (let ((buffer (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buffer
                  (goto-char (point-max))
                  (insert "(def a 1)\n")
                  (let ((replique-test--answer nil))
                    (replique-test--loading
                      (replique-load-file)
                      (should asked)
                      (should (buffer-modified-p))
                      (should-not (string-match-p
                                   "(def a 1)"
                                   (with-temp-buffer (insert-file-contents file)
                                                     (buffer-string))))
                      (should (string-match-p (regexp-quote file) sent)))))
              (set-buffer-modified-p nil)
              (kill-buffer buffer))))
      (delete-file file))))

(ert-deftest replique-test-a-file-not-written-yet-is-saved-and-then-loaded ()
  "Saving is what puts the file there, so the offer comes before the file is
looked for: a buffer of a name nobody has written yet is a file as soon as it
is saved, and refusing it first would refuse the one case the offer fixes."
  (let ((file (expand-file-name "replique-test-brandnew.clj" temporary-file-directory)))
    (when (file-exists-p file) (delete-file file))
    (unwind-protect
        (let ((buffer (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buffer
                (insert "(ns replique.test-brandnew)\n")
                (should-not (file-exists-p file))
                (let ((replique-test--answer t))
                  (replique-test--loading
                    (replique-load-file)
                    (should (file-exists-p file))
                    (should (string-match-p (regexp-quote file) sent)))))
            (set-buffer-modified-p nil)
            (kill-buffer buffer)))
      (when (file-exists-p file) (delete-file file)))))

(ert-deftest replique-test-the-reload-directive-asks-for-everything-that-changed ()
  "The client names no file: which ones changed is the process's own
question to answer.  What it does say is how long a runtime may take, which
is the one thing the process cannot work out for itself - a reload sent from
here is a reload a command sent, and it must not be able to take Emacs with
it."
  (let ((replique-reload-timeout 30000))
    (should (equal "#replique/reload {:timeout 30000}" (replique-reload-directive))))
  (let ((replique-reload-timeout nil))
    (should (equal "#replique/reload {}" (replique-reload-directive)))))

(ert-deftest replique-test-only-modified-clojure-buffers-are-offered-before-a-reload ()
  "The process reads the disk, so a buffer with unsaved changes holds
nothing a reload can see - and only the Clojure ones are offered: what is
being loaded is Clojure, and a note in some other buffer has nothing to do
with it.  A buffer holding no file is no file to reload either."
  (let ((offered nil)
        (clj (expand-file-name "replique-test-offered.clj" temporary-file-directory))
        (txt (expand-file-name "replique-test-offered.txt" temporary-file-directory)))
    (unwind-protect
        (progn
          (with-temp-file clj (insert "(ns replique.test-offered)\n"))
          (with-temp-file txt (insert "a note\n"))
          (let ((clj-buffer (find-file-noselect clj))
                (txt-buffer (find-file-noselect txt)))
            (unwind-protect
                (progn
                  (cl-letf (((symbol-function 'save-some-buffers)
                             (lambda (_arg pred) (setq offered pred)))
                            ((symbol-function 'replique-repl-ensure-here) (lambda () 'a-repl))
                            ((symbol-function 'replique-repl-send-code)
                             (lambda (&rest _) nil)))
                    (replique-reload-all))
                  (should (functionp offered))
                  (should (with-current-buffer clj-buffer (funcall offered)))
                  (should-not (with-current-buffer txt-buffer (funcall offered)))
                  (should-not (with-temp-buffer (replique-clojure-mode)
                                                (funcall offered))))
              (kill-buffer clj-buffer)
              (kill-buffer txt-buffer))))
      (delete-file clj)
      (delete-file txt))))

(ert-deftest replique-test-reloading-asks-the-process-what-changed ()
  "And the process answers, whichever it is.  One whose compiler wrote down
what it compiled answers with the files it loaded; one that did not cannot
know what changed, and says what to start it on instead of answering that
nothing did.

Either way the answer reaches the buffer that asked: the command is run
from a source buffer, and what it did is news there rather than in a repl
buffer nobody is looking at.

The buffers are not saved here - that is its own test, and asking in batch
would be asking nobody."
  (replique-test-with-repl repl
    (let ((echoed (replique-test-message
                    (with-temp-buffer
                      (replique-clojure-mode)
                      (setq-local replique-current-repl repl)
                      (cl-letf (((symbol-function 'save-some-buffers)
                                 (lambda (&rest _) nil)))
                        (replique-reload-all)))
                    (replique-test-wait-for
                     (lambda () (replique-repl--at-prompt repl))))))
      (should (string-match-p (regexp-quote "#replique/reload")
                              (replique-test-text repl)))
      (should echoed)
      (if (string-match-p "keep track of what it compiled" echoed)
          (should (string-match-p "clojure.analysis" echoed))
        (should (string-match-p "\\[.*\\]\\'" (string-trim echoed)))))
    (should (string-match-p "^2$" (replique-test-eval repl "(+ 1 1)")))))

(ert-deftest replique-test-a-buffer-holding-no-file-is-not-loaded ()
  "Said as what it is.  A buffer that holds no file and a file that is not
there are two different things to be told, and a command that only has to
fail somehow passes for either."
  (with-temp-buffer
    (replique-clojure-mode)
    (insert "(def a 1)\n")
    (let ((err (should-error (replique-load-file) :type 'user-error)))
      (should (string-match-p "holds no file to load" (cadr err))))))

(ert-deftest replique-test-a-file-that-is-not-there-is-not-loaded ()
  "The process reads the file off the disk, so a buffer visiting a name
nobody has written yet fails here, where the name is, rather than there."
  (let ((file (expand-file-name "replique-test-absent.clj" temporary-file-directory)))
    (when (file-exists-p file) (delete-file file))
    (let ((buffer (find-file-noselect file)))
      (unwind-protect
          (with-current-buffer buffer
            (let ((err (should-error (replique-load-file) :type 'user-error)))
              (should (string-match-p (regexp-quote file) (cadr err)))))
        (kill-buffer buffer)))))

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

(ert-deftest replique-test-a-thread-that-threw-says-nothing-where-it-is-shown ()
  "What a thread threw is output like any other: it goes to the buffer of
the process, and that buffer being on screen is what makes saying it in
the echo area a second way of saying what is already said."
  (let ((process (replique-test-process)))
    (replique-test-with-repl repl
      (let ((buffer (replique-process-buffer process)))
        (replique-test-with-shown buffer
          (let ((said (replique-test-messages
                        (replique-test-eval
                         repl (concat "(do (.start (Thread. (fn [] (throw"
                                      " (Exception. \"SHOWN\"))))) :started)"))
                        (replique-test-wait-for
                         (lambda ()
                           (with-current-buffer buffer
                             (string-match-p "SHOWN" (buffer-string))))
                         10))))
            (should-not (seq-some (lambda (one) (string-match-p "SHOWN" one)) said))))))))

(ert-deftest replique-test-a-thread-that-threw-is-said-where-it-is-not ()
  "And the other half of it: a buffer nobody is looking at is where an
exception goes to not be read, which is the case the echo area is for."
  (let ((process (replique-test-process)))
    (replique-test-with-repl repl
      (let ((replique--unread nil)
            (global-mode-string nil)
            (replique--echo-pending nil)
            (replique--echo-timer nil)
            (buffer (replique-process-buffer process)))
        (unwind-protect
            (progn
              (replique-test-hide buffer)
              (replique-test-eval
               repl (concat "(do (.start (Thread. (fn [] (throw"
                            " (Exception. \"UNSEEN\"))))) :started)"))
              (should (replique-test-wait-for
                       (lambda ()
                         (string-match-p "UNSEEN" (or (cdr replique--echo-pending) "")))
                       10))
              (should (string-match-p
                       "UNSEEN"
                       (replique-test-message (replique--echo-flush)))))
          (when replique--echo-timer (cancel-timer replique--echo-timer)))))))

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
      (should-not (assq buffer replique--unread))
      ;; at the end of a line: the code the buffer was sent is echoed into
      ;; it, and holds the string the future is about to print.  What the
      ;; future prints lands after the prompt, which is where output that
      ;; nobody is waiting for lands
      (should (replique-test-wait-for
               (lambda () (string-match-p "LATE$" (replique-test-text repl))) 10))
      (should (assq buffer replique--unread)))))

(ert-deftest replique-test-what-a-future-printed-is-said-where-it-is-seen ()
  "The whole way, with a process at the end of it: a thread of it prints
long after the form that started it was answered, the frame arrives at a
buffer no window shows, and what it printed is what the echo area is
handed.  Handed rather than shown, because what shows it is an idle timer
and a batch editor is never idle."
  (replique-test-with-repl repl
    (let ((replique--unread nil)
          (global-mode-string nil)
          (replique--echo-pending nil)
          (replique--echo-timer nil)
          (buffer (replique-repl--buffer repl)))
      (unwind-protect
          (progn
            (replique-test-hide buffer)
            (replique-repl-send-code
             repl "(do (future (Thread/sleep 500) (println \"AFTERWARDS\")) :started)"
             nil t)
            (should (replique-test-wait-for
                     (lambda ()
                       (string-match-p "AFTERWARDS" (or (cdr replique--echo-pending) "")))
                     10))
            (should (string-match-p
                     "AFTERWARDS"
                     (replique-test-message (replique--echo-flush)))))
        (when replique--echo-timer (cancel-timer replique--echo-timer))))))

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
          (should (assq buffer replique--unread))
          ;; named by what tells it apart, not by the decoration a buffer
          ;; list needs to tell it from a file
          (should (equal " tracked (1)" (replique-unread-mode-line)))
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
  (let* ((buffer (generate-new-buffer "*replique: long*")))
    (unwind-protect
        (let ((lines (replique-echo-shorten
                      (mapconcat #'number-to-string (number-sequence 1 100) "\n")
                      buffer))
              (long (replique-echo-shorten (make-string 5000 ?x) buffer)))
          (should (equal replique-echo-max-lines
                         (length (split-string lines "\n"))))
          (should (string-match-p "see \\*replique: long\\*" lines))
          (should (< (length long) 5000))
          (should (string-match-p "see \\*replique: long\\*" long))
          ;; and a short one is left alone
          (should (equal "nil" (replique-echo-shorten "nil" buffer))))
      (kill-buffer buffer))))

(ert-deftest replique-test-what-is-kept-for-the-echo-area-is-bounded ()
  "A form printing in a loop must not be accumulated in full for the sake
of the ten lines of it that will be shown."
  (let ((repl (replique-repl--make :to-echo 1)))
    (dotimes (_ 100)
      (replique-repl--echo-keep repl (make-string 1000 ?x)))
    (should (<= (length (replique-repl--echoed repl))
                (1+ replique-echo-max-chars)))
    ;; kept past what is shown, which is what tells a cut one from a whole one
    (should (> (length (replique-repl--echoed repl)) replique-echo-max-chars))))

(ert-deftest replique-test-nothing-is-kept-for-a-buffer-that-is-not-waiting ()
  (let ((repl (replique-repl--make :to-echo 0)))
    (replique-repl--echo-keep repl "printed")
    (should-not (replique-repl--echoed repl))))

(defmacro replique-test-with-unseen (name &rest body)
  "Run BODY with NAME bound to a buffer no window shows, and nothing owed.

A buffer nothing has been done with is a buffer no window shows, which is
the state every one of these is about."
  (declare (indent 1))
  `(let ((replique--unread nil)
         (global-mode-string nil)
         (replique--echo-pending nil)
         (replique--echo-timer nil)
         (replique-echo-awaited-function nil)
         (,name (generate-new-buffer "*replique: unseen*")))
     (unwind-protect (progn ,@body)
       (when replique--echo-timer (cancel-timer replique--echo-timer))
       (kill-buffer ,name))))

(ert-deftest replique-test-what-arrived-unseen-is-said-in-the-echo-area ()
  "The mode line waits to be read, which is no use to somebody who never
looks down there.  What arrived is said where what a form produced is
said, because that is where somebody is already looking."
  (replique-test-with-unseen buffer
    (replique-insert-output buffer "LATE\n")
    (should (equal "LATE" (replique-test-message (replique--echo-flush))))))

(ert-deftest replique-test-a-burst-of-writes-is-one-message ()
  "Output arrives in whatever chunks the operating system handed over, so
a message per chunk is the last fragment of a line flashing past where a
line was wanted."
  (replique-test-with-unseen buffer
    (replique-insert-output buffer "one\n")
    (replique-insert-output buffer "two\n")
    (replique-insert-output buffer "three\n")
    (should (equal "one\ntwo\nthree"
                   (replique-test-message (replique--echo-flush))))))

(ert-deftest replique-test-nothing-is-said-about-a-buffer-on-screen ()
  "A buffer somebody is looking at has already said it."
  (let ((replique--unread nil)
        (global-mode-string nil)
        (replique--echo-pending nil)
        (replique--echo-timer nil)
        (buffer (generate-new-buffer "*replique: shown*"))
        (shown (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (set-window-buffer (selected-window) buffer)
          (replique-insert-output buffer "SEEN\n")
          (should-not replique--echo-pending)
          (should-not (replique-test-message (replique--echo-flush))))
      (when (buffer-live-p shown)
        (set-window-buffer (selected-window) shown))
      (kill-buffer buffer))))

(ert-deftest replique-test-the-echo-area-is-left-to-what-was-asked-for ()
  "An evaluation is about to report where it was asked from.  A line a
background thread printed in the meantime does not get to be the last
thing said - and it is not lost by not being said, because the mode line
holds the way to it."
  (replique-test-with-unseen buffer
    (let ((replique-echo-awaited-function (lambda () t)))
      (replique-insert-output buffer "MEANWHILE\n")
      (should-not (replique-test-message (replique--echo-flush)))
      (should (assq buffer replique--unread)))))

(ert-deftest replique-test-a-buffer-read-before-the-timer-fires-says-nothing ()
  "Between the writing and the saying is a fifth of a second, and a window
can come to show the buffer inside it."
  (let ((replique--unread nil)
        (global-mode-string nil)
        (replique--echo-pending nil)
        (replique--echo-timer nil)
        (replique-echo-awaited-function nil)
        (buffer (generate-new-buffer "*replique: opened*"))
        (shown (window-buffer (selected-window))))
    (unwind-protect
        (progn
          (replique-insert-output buffer "GONE\n")
          (set-window-buffer (selected-window) buffer)
          (should-not (replique-test-message (replique--echo-flush))))
      (when (buffer-live-p shown)
        (set-window-buffer (selected-window) shown))
      (kill-buffer buffer))))

(ert-deftest replique-test-what-is-gathered-for-the-echo-area-is-bounded ()
  "A thread printing in a loop must not be accumulated in full for the
sake of the ten lines of it that will be shown."
  (replique-test-with-unseen buffer
    (dotimes (_ 100)
      (replique-insert-output buffer (make-string 1000 ?x)))
    (should (<= (length (cdr replique--echo-pending))
                (1+ replique-echo-max-chars)))
    ;; gathered past what is shown, which is what tells a cut one from a
    ;; whole one
    (should (> (length (cdr replique--echo-pending)) replique-echo-max-chars))))

(ert-deftest replique-test-two-buffers-are-two-messages ()
  "One message made of the two of them would be a message saying that one
process printed what another one printed."
  (replique-test-with-unseen one
    (let ((two (generate-new-buffer "*replique: other*")))
      (unwind-protect
          (progn
            (should (equal "AAA" (replique-test-message
                                   (replique-insert-output one "AAA\n")
                                   (replique-insert-output two "BBB\n"))))
            (should (equal "BBB" (replique-test-message (replique--echo-flush)))))
        (kill-buffer two)))))

(ert-deftest replique-test-a-line-on-standard-error-reads-as-one ()
  "Which stream it came on is something to see rather than something to
read, and it is the face it went into the buffer in."
  (replique-test-with-unseen buffer
    (replique-insert-output buffer "BROKE\n" 'replique-stderr)
    (let ((said (replique-test-message (replique--echo-flush))))
      (should (equal "BROKE" said))
      (should (eq 'replique-stderr (get-text-property 0 'face said))))))

(ert-deftest replique-test-the-mode-line-counts-what-came ()
  "A mark that appeared once and then stood still says the same thing
whether one line came or a thousand.  A number that moves is what is seen
out of the corner of an eye."
  (replique-test-with-unseen buffer
    (replique-insert-output buffer "one\ntwo\nthree\n")
    (should (equal " unseen (3)" (replique-unread-mode-line)))
    (replique-insert-output buffer "four\nfive\n")
    (should (equal " unseen (5)" (replique-unread-mode-line)))))

(ert-deftest replique-test-a-line-that-has-not-ended-counts-as-one ()
  "What is counted is newlines, and the first thing a process writes is a
line it has not ended yet.  Nought would read as nothing having arrived."
  (replique-test-with-unseen buffer
    (replique-insert-output buffer "no newline here")
    (should (equal " unseen (1)" (replique-unread-mode-line)))))

(ert-deftest replique-test-an-idle-editor-is-what-says-it ()
  "Nothing calls the flush: being idle is what says the echo area is free,
and it is also what says the burst is over.  One timer for the burst,
because a timer armed again per chunk is a burst that is never over."
  (replique-test-with-unseen buffer
    (replique-insert-output buffer "IDLY\n")
    (let ((armed replique--echo-timer))
      (should (memq armed timer-idle-list))
      (should (eq #'replique--echo-flush (timer--function armed)))
      (replique-insert-output buffer "AND SO\n")
      (should (eq armed replique--echo-timer))
      (should (equal "IDLY\nAND SO"
                     (replique-test-message (funcall (timer--function armed)))))
      (should-not replique--echo-pending))))

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

(ert-deftest replique-test-the-standard-input-of-the-process-is-not-the-repl ()
  "What a repl reads is a socket.  The standard input of the jvm is
another thing entirely - it is what `java.io.Console' reads, which is
where a keystore passphrase is asked for, before any repl exists - and
nothing typed at a repl reaches it."
  (replique-test-with-repl repl
    (replique-process-input "from-emacs")
    (replique-test-settle)
    (replique-repl-send-code
     repl "(.readLine (java.io.BufferedReader. (java.io.InputStreamReader. System/in)))")
    (should (replique-test-wait-for
             (lambda () (string-match-p "\"from-emacs\"" (replique-test-text repl)))))))

(ert-deftest replique-test-a-password-for-the-process-is-not-read-out-loud ()
  "A command of its own rather than an argument to
`replique-process-input': a password echoed because the argument was
forgotten is a password that has already been echoed."
  (replique-test-with-repl repl
    (let ((echoed nil)
          (hidden nil))
      (cl-letf (((symbol-function 'read-string)
                 (lambda (&rest _) (setq echoed t) "shown"))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _) (setq hidden t) "hidden")))
        (call-interactively #'replique-process-input-password))
      (should hidden)
      (should-not echoed))
    (replique-test-settle)
    (replique-repl-send-code
     repl "(.readLine (java.io.BufferedReader. (java.io.InputStreamReader. System/in)))")
    (should (replique-test-wait-for
             (lambda () (string-match-p "\"hidden\"" (replique-test-text repl)))))))

(ert-deftest replique-test-a-process-emacs-did-not-start-has-no-input-here ()
  "Its standard input belongs to whatever started it - a shell, or an
Emacs that has since restarted.  Saying so is what stops a passphrase
being typed into nothing."
  (should (equal '(user-error "Emacs did not start this process - its input is not here")
                 (should-error (replique-process--stdin
                                (replique-process--make :id "outside")))))
  (let ((proc (make-process :name "replique-test-gone" :buffer nil
                            :command (list "cat") :noquery t)))
    (delete-process proc)
    (should (equal '(user-error "The process is gone")
                   (should-error (replique-process--stdin
                                  (replique-process--make :id "gone" :proc proc)))))))

(ert-deftest replique-test-a-process-that-will-not-stop-says-so ()
  "A process Emacs did not start that does not answer is a process this
command cannot stop.  What it must not do then is behave the way
`replique-disconnect' does under the name that promises the opposite: a
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
          (let ((said (cl-letf (((symbol-function 'replique-process--ask-to-stop)
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

(defun replique-test-listener (kind)
  "Return a server on this machine that accepts a connection and says nothing.

KIND is `silent\\=' for one that leaves the connection open and `rude\\=' for
one that closes it at once.  Either is what a port file can end up naming:
the process that wrote it is gone, the port has been handed out again, and
what holds it now does not speak replique.  A port that accepts is a port
that does not look dead, whatever is behind it."
  (make-network-process
   :name "replique-test-listener" :server t :host "127.0.0.1" :service t
   :noquery t :filter #'ignore
   :log (lambda (_server client _message)
          (when (eq kind 'rude) (delete-process client)))))

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
      ;; A port somewhere else that takes the connection and then says
      ;; nothing says no more than an unreachable one does: a host of its
      ;; own can be slow, or behind something that accepts for it
      (replique-process--reap file (replique-process--description file) 'unanswered)
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

(ert-deftest replique-test-a-port-that-answers-nothing-is-a-port-file-that-is-wrong ()
  "A port that was taken over by something that is not replique refuses
nothing: the handshake goes out and no answer ever comes back.  Without a
deadline the connect waits for it for the rest of the session - saying
nothing, connecting to nothing, and leaving the file that sent it there to
be offered again.  The file is what has to go."
  (let ((listener (replique-test-listener 'silent))
        (replique-conn-handshake-timeout 1))
    (unwind-protect
        (replique-test-with-project dir
          (let ((file (replique-test-write-port-file
                       dir (list :process-id "silent" :host "127.0.0.1"
                                 :port (process-contact listener :service)
                                 :directory (directory-file-name dir)
                                 :pid 1 :started-at 1))))
            (replique-connect dir)
            (should (replique-test-wait-for (lambda () (not (file-exists-p file))) 10))
            ;; and nothing was connected to: a port that says nothing is
            ;; not a process, however long the socket stayed open
            (should-not (replique-process-in dir))))
      (delete-process listener))))

(ert-deftest replique-test-a-port-that-drops-the-connection-is-a-port-file-that-is-wrong ()
  "The other way a port that is not replique answers: it takes the
connection and closes it.  Nothing was ever connected to, so this is not a
process going away - it is the file being wrong, and saying so is the only
thing that makes the directory usable again."
  (let ((listener (replique-test-listener 'rude)))
    (unwind-protect
        (replique-test-with-project dir
          (let ((file (replique-test-write-port-file
                       dir (list :process-id "rude" :host "127.0.0.1"
                                 :port (process-contact listener :service)
                                 :directory (directory-file-name dir)
                                 :pid 1 :started-at 1))))
            (replique-connect dir)
            (should (replique-test-wait-for (lambda () (not (file-exists-p file))) 10))
            ;; and nothing was connected to: a port that says nothing is
            ;; not a process, however long the socket stayed open
            (should-not (replique-process-in dir))))
      (delete-process listener))))

;;; Stopping a process

(ert-deftest replique-test-a-process-that-is-stopped-takes-its-port-file-with-it ()
  "A process is asked to stop rather than killed outright, so that the
shutdown hook deleting its port file runs.  A file left behind is a
process `replique-connect' goes on offering."
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
          ;; the wait is the command's, not the test's: what it says it did
          ;; is done when it returns
          (replique-kill-process process)
          (should-not (file-exists-p port-file)))))))

(ert-deftest replique-test-a-request-nobody-will-answer-is-answered ()
  "A connection that dies takes every unanswered request with it.  A caller
that only ever hears back on success is a command that silently does
nothing when the process is gone."
  (replique-test-with-repl repl
    (let* ((conn (replique-repl--conn repl))
           (answers nil))
      (replique-conn-request conn (list :op :process-info)
                             (lambda (frame) (push frame answers)))
      (replique-conn-close conn)
      (should (replique-test-wait-for (lambda () answers) 5))
      (let ((frame (car answers)))
        (should (equal "error" (plist-get frame :tag)))
        (should (equal replique-conn-closed-error (plist-get frame :error)))))))

(ert-deftest replique-test-a-repl-on-a-process-that-went-says-so ()
  "The control connection says when there is nothing to connect to.  A repl
is opened later, and the process can have left in between - which used to
reach the developer as a backtrace.  The buffer it had made goes too:
a repl buffer of a repl that never opened is a buffer that says nothing."
  (let* ((process (replique-process--make :id "replique-test-gone"
                                          :host "127.0.0.1"
                                          :port (replique-test-free-port)))
         (name (replique-repl--buffer-name process nil nil)))
    (should-error (replique-repl process) :type 'user-error)
    (should-not (get-buffer name))))

;;; Cleaning up

(defun replique-test-tear-down ()
  "Stop the process the tests shared."
  (when (replique-process-live-p replique-test-process)
    (replique-kill-process replique-test-process))
  (setq replique-test-process nil))

(add-hook 'kill-emacs-hook #'replique-test-tear-down)

(provide 'replique-test)

;;; replique-test.el ends here
