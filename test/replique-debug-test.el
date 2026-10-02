;;; replique-debug-test.el --- Tests for a stopped thread  -*- lexical-binding: t; -*-

;;; Commentary:

;; What is shown of a thread that stopped, and what the commands ask the
;; process, without a process: the requests are kept and answered by the
;; test.  What a real process does is in the test at the bottom, which needs
;; one - started for it, since a thread stops only in a jvm started with the
;; JDWP agent.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-debug)

(defvar replique-debug-test--asked nil
  "The requests not answered yet, oldest first: (MSG . CALLBACK).")

(defmacro replique-debug-test--with (&rest body)
  "Run BODY with a process that keeps what it is asked, to be answered."
  (declare (indent 0))
  `(let ((replique-debug-test--asked nil)
         (process (replique-process--make :id "debug-test" :directory "/tmp/")))
     (ignore process)
     (cl-letf (((symbol-function 'replique-process-live-p) (lambda (p) (and p t)))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional callback)
                  (setq replique-debug-test--asked
                        (append replique-debug-test--asked (list (cons msg callback))))
                  nil)))
       (unwind-protect (save-window-excursion ,@body)
         (replique-debug--forget-arrow)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'replique-debug-mode 'replique-inspect-mode)
               (let ((kill-buffer-hook nil)) (kill-buffer buffer)))))))))

(defun replique-debug-test--asked (op)
  "Return the oldest request for OP not answered yet, with its callback."
  (seq-find (lambda (cell) (eq op (plist-get (car cell) :op)))
            replique-debug-test--asked))

(defun replique-debug-test--answer (op frame)
  "Answer the oldest request for OP with FRAME, and return what it asked."
  (let ((cell (or (replique-debug-test--asked op) (error "Nothing asked %s" op))))
    (setq replique-debug-test--asked (delq cell replique-debug-test--asked))
    (when (cdr cell) (funcall (cdr cell) frame))
    (car cell)))

(defvar replique-debug-test--file nil
  "A source a thread stops in.")

(defmacro replique-debug-test--with-source (&rest body)
  "Run BODY with `replique-debug-test--file' holding a few lines of code."
  (declare (indent 0))
  `(let ((replique-debug-test--file (make-temp-file "replique-debug-test" nil ".clj")))
     (unwind-protect
         (progn
           (with-temp-file replique-debug-test--file
             (insert "(ns user)\n(defn f [n]\n  (let [a (inc n)]\n    (replique.debug/break!)\n    a))\n"))
           ,@body)
       (when-let* ((buffer (find-buffer-visiting replique-debug-test--file)))
         (kill-buffer buffer))
       (delete-file replique-debug-test--file))))

(defun replique-debug-test--stop (process)
  "Tell PROCESS's editor that thread 41 stopped at line 4 of the source."
  (replique-debug--event process
                         (list :tag "event" :event "debug-paused" :thread 41
                               :name "worker" :ns "user"
                               :file replique-debug-test--file :line 4 :column 5)))

(defun replique-debug-test--frames ()
  "The frames the process answers with."
  (list :tag "reply"
        :frames (list (list :index 0 :fn "user/f" :class "user$f" :method "invokeStatic"
                            :file replique-debug-test--file :line 4)
                      (list :index 1 :class "clojure.lang.Compiler" :method "eval"
                            :source "clojure/lang/Compiler.java" :line 7757)
                      (list :index 2 :fn "user/g" :class "user$g" :method "invokeStatic"
                            :file replique-debug-test--file :line 9))))

(defun replique-debug-test--text ()
  "Return what the current buffer shows."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest replique-debug-test-a-thread-that-stops-is-shown ()
  "Where it stopped, with an arrow; its frames; and the locals of the frame
that stopped."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (let ((buffer (replique-debug--buffer-of process 41)))
        (should buffer)
        (with-current-buffer buffer
          (should (eq 41 replique-debug--thread))
          (should (string-match-p "stopped at replique-debug-test.*\\.clj:4"
                                  (replique-debug--header)))
          (should (equal '(:op :debug-frames :thread 41)
                         (replique-debug-test--answer :debug-frames
                                                      (replique-debug-test--frames))))
          (let ((text (replique-debug-test--text)))
            (should (string-match-p "0  user/f" text))
            (should (string-match-p "2  user/g" text))
            (should-not (string-match-p "Compiler" text))
            (should (string-match-p "1 frames of the host" text)))
          (replique-debug-toggle-host-frames)
          (should (string-match-p "clojure.lang.Compiler.eval" (replique-debug-test--text)))))
      ;; the arrow, on the line it stopped on
      (should (markerp replique-debug--arrow))
      (with-current-buffer (marker-buffer replique-debug--arrow)
        (should (= 4 (line-number-at-pos replique-debug--arrow))))
      (should (equal '(:debug (:thread 41 :frame 0))
                     (plist-get (car (replique-debug-test--asked :inspect)) :source))))))

(ert-deftest replique-debug-test-the-commands-name-the-thread-and-the-frame ()
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (with-current-buffer (replique-debug--buffer-of process 41)
        (replique-debug-test--answer :debug-frames (replique-debug-test--frames))
        (goto-char (point-min))
        (search-forward "user/g")
        (replique-debug-restart)
        (should (equal '(:op :debug-restart :thread 41 :frame 2)
                       (car (replique-debug-test--asked :debug-restart))))
        (replique-debug-eval "(+ a 1)")
        (should (equal '(:op :debug-eval :thread 41 :frame 2 :code "(+ a 1)")
                       (car (replique-debug-test--asked :debug-eval))))
        (replique-debug-continue)
        (should (equal '(:op :debug-continue :thread 41)
                       (car (replique-debug-test--asked :debug-continue))))
        (replique-debug-abort)
        (should (equal '(:op :debug-continue :thread 41 :abort t)
                       (car (car (last (seq-filter (lambda (cell)
                                                (eq :debug-continue (plist-get (car cell) :op)))
                                              replique-debug-test--asked))))))))))

(ert-deftest replique-debug-test-a-value-is-said ()
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (with-current-buffer (replique-debug--buffer-of process 41)
        (replique-debug-eval "(+ a 1)")
        (replique-debug-test--answer :debug-eval '(:tag "reply" :evaluation 7))
        (should (equal "2" (replique-test-message
                             (replique-debug--event
                              process '(:tag "event" :event "debug-evaluated"
                                             :thread 41 :evaluation 7 :value "2")))))
        ;; an evaluation is answered once
        (should-not (replique-test-message
                       (replique-debug--event
                        process '(:tag "event" :event "debug-evaluated"
                                       :thread 41 :evaluation 7 :value "2"))))))))

(ert-deftest replique-debug-test-a-thread-that-goes-on-is-shown-running ()
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (replique-debug--event process '(:tag "event" :event "debug-resumed" :thread 41))
      (should-not replique-debug--arrow)
      (with-current-buffer (replique-debug--buffer-of process 41)
        (should (string-match-p "Running" (replique-debug-test--text)))
        (should (string-match-p "running" (replique-debug--header)))
        (should-error (replique-debug-continue) :type 'user-error)))))

(ert-deftest replique-debug-test-a-thread-that-is-not-stopped-any-more-says-so ()
  "The process is the one that knows: an answer saying the thread is not
stopped is a thread shown running."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (with-current-buffer (replique-debug--buffer-of process 41)
        (replique-debug-test--answer :debug-frames
                                     '(:tag "error" :error "not-paused"
                                            :message "Thread 41 is not stopped"))
        (should-not replique-debug--pause)
        (should (string-match-p "Running" (replique-debug-test--text)))))))

(ert-deftest replique-debug-test-the-jvm-is-started-for-it ()
  (let ((replique-coordinates nil)
        (replique-aliases nil)
        (replique-user-aliases nil))
    (let ((replique-debugger t))
      (let ((command (replique-process--command "/tmp/a-project/")))
        (should (seq-some (lambda (arg) (string-prefix-p "-J-agentlib:jdwp=" arg)) command))
        ;; locals are cleared, as Clojure does by default
        (should-not (seq-some (lambda (arg) (string-match-p "locals-clearing" arg)) command))
        ;; options of the jvm, so before the main option
        (should (< (seq-position command (seq-find (lambda (arg) (string-prefix-p "-J-" arg))
                                                   command))
                   (seq-position command "-M")))))
    ;; asked for by the start itself
    (let ((replique-debugger nil))
      (should (seq-some (lambda (arg) (string-prefix-p "-J-agentlib:jdwp=" arg))
                        (replique-process--command "/tmp/a-project/" nil t))))
    (let ((replique-debugger nil))
      (should-not (seq-some (lambda (arg) (string-prefix-p "-J-" arg))
                            (replique-process--command "/tmp/a-project/"))))))

(ert-deftest replique-debug-test-a-restart-of-code-that-clears-its-locals-is-asked-again ()
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug--event process
                             (list :tag "event" :event "debug-paused" :thread 41
                                   :name "worker" :ns "user" :locals-cleared t
                                   :file replique-debug-test--file :line 4))
      (with-current-buffer (replique-debug--buffer-of process 41)
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
          (should-error (replique-debug-restart) :type 'user-error))
        (should-not (replique-debug-test--asked :debug-restart))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
          (replique-debug-restart))
        (should (replique-debug-test--asked :debug-restart))))))

(ert-deftest replique-debug-test-locals-are-kept-or-cleared-from-now-on ()
  (replique-debug-test--with
    (let ((asked nil))
      (cl-letf (((symbol-function 'replique-name-process) (lambda () process))
                ((symbol-function 'replique-process-request-sync)
                 (lambda (_process msg &rest _) (push msg asked) '(:tag "reply" :clear nil))))
        (with-temp-buffer
          (replique-debug-keep-locals)
          (replique-debug-keep-locals t)))
      (should (equal '((:op :locals-clearing :clear false)
                       (:op :locals-clearing :clear t))
                     (reverse asked))))))

;;; Against a process

(defun replique-debug-test--locals-text (thread)
  "Return what the view of the locals of the frame 0 of THREAD shows."
  (when-let* ((buffer (seq-find (lambda (buffer)
                                  (with-current-buffer buffer
                                    (and (derived-mode-p 'replique-inspect-mode)
                                         (equal thread (plist-get (plist-get replique-inspect--source :debug)
                                                                  :thread)))))
                                (buffer-list))))
    (with-current-buffer buffer (replique-debug-test--text))))

(ert-deftest replique-debug-test-a-thread-stops-is-fixed-and-goes-on ()
  "Stopped by break!, looked at, the function it calls redefined, the call
started over, and let go of - the repl gets the value of the fixed code."
  (replique-test-project)
  (replique-test-with-project dir
    (with-temp-file (expand-file-name "deps.edn" dir) (insert "{:paths [\"src\"]}"))
    (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
           (replique-debugger nil)
           (process (let ((known replique-processes))
                      (replique-process-start dir nil nil t)
                      (unless (replique-test-wait-for
                               (lambda () (seq-difference replique-processes known)) 120)
                        (error "The process did not start"))
                      (replique-test--note-process
                       (car (seq-difference replique-processes known)))))
           (repl nil))
      (unwind-protect
          (save-window-excursion
            (setq repl (replique-repl process))
            (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 60))
            ;; kept by what is compiled from here, which a restart needs
            (with-current-buffer (replique-repl--buffer repl)
              (replique-debug-keep-locals))
            (replique-test-eval repl "(defn g [x] (* 10 x))")
            (replique-test-eval repl "(defn f [n] (let [a (inc n) b (g a)] (replique.debug/break!) (+ a b)))")
            (replique-repl-send-code repl "(f 1)")
            (let ((buffer nil))
              (should (replique-test-wait-for
                       (lambda ()
                         (setq buffer (seq-find (lambda (b)
                                                  (with-current-buffer b
                                                    (and (derived-mode-p 'replique-debug-mode)
                                                         replique-debug--pause)))
                                                (buffer-list))))
                       120))
              (with-current-buffer buffer
                (should (replique-test-wait-for
                         (lambda () (string-match-p "user/f" (replique-debug-test--text)))
                         30))
                (should (string-match-p "evaluating for" (replique-debug--header)))
                (let ((thread replique-debug--thread))
                  (should (replique-test-wait-for
                           (lambda () (string-match-p "b  20" (or (replique-debug-test--locals-text thread) "")))
                           30))
                  (should (equal "1022"
                                 (let ((said nil))
                                   (cl-letf (((symbol-function 'message)
                                              (lambda (format &rest args)
                                                (setq said (apply #'format format args)))))
                                     (replique-debug-eval "(+ a b 1000)")
                                     (replique-test-wait-for (lambda () (equal said "1022")) 30))
                                   said)))
                  (replique-debug-eval "(defn g [x] (* 100 x))")
                  (replique-test-settle 1)
                  (goto-char (point-min))
                  (replique-debug-restart)
                  (should (replique-test-wait-for
                           (lambda () (string-match-p "b  200" (or (replique-debug-test--locals-text thread) "")))
                           30))
                  (replique-debug-continue)
                  (should (replique-test-wait-for
                           (lambda () (string-match-p "202" (replique-test-text repl)))
                           30))
                  (should (replique-test-wait-for (lambda () (null replique-debug--pause)) 10))))))
        (when repl
          (replique-conn-close (replique-repl--conn repl))
          (kill-buffer (replique-repl--buffer repl)))
        (replique-kill-process process)))))

(provide 'replique-debug-test)

;;; replique-debug-test.el ends here
