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
         (maphash (lambda (_ timer) (cancel-timer timer)) replique-debug--leaving)
         (clrhash replique-debug--leaving)
         (clrhash replique-debug--opened)
         (maphash (lambda (_ o) (delete-overlay o)) replique-debug--units)
         (clrhash replique-debug--units)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             ;; or made for a view, by a test that does not show one
             (when (or (derived-mode-p 'replique-debug-mode 'replique-inspect-mode)
                       (string-prefix-p "*replique-recorded " (buffer-name)))
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
        (replique-debug-continue)
        (should (equal '(:op :debug-continue :thread 41)
                       (car (replique-debug-test--asked :debug-continue))))
        (replique-debug-abort)
        (should (equal '(:op :debug-continue :thread 41 :abort t)
                       (car (car (last (seq-filter (lambda (cell)
                                                (eq :debug-continue (plist-get (car cell) :op)))
                                              replique-debug-test--asked))))))))))

(ert-deftest replique-debug-test-a-repl-evaluates-in-the-frame-at-point ()
  "Opened on the frame, named after the thread, made the repl the commands
act on - pointed at another frame, rather than opened again - and gone with
the thread."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (let* ((opened nil)
             (directives nil)
             (replique-current-repl nil)
             (buffer (generate-new-buffer "*replique: debug-test clj*"))
             (repl (replique-repl--make :process process :buffer buffer)))
        (cl-letf (((symbol-function 'replique-repl--open)
                   (lambda (&rest args)
                     (setq opened args)
                     (with-current-buffer buffer (setq-local replique--buffer-repl repl))
                     (setq replique-current-repl repl)
                     repl))
                  ((symbol-function 'replique-repl-live-p) (lambda (r) (eq r repl)))
                  ((symbol-function 'replique-repl-send-directive)
                   (lambda (_repl directive) (push directive directives))))
          (with-current-buffer (replique-debug--buffer-of process 41)
            (replique-debug-test--answer :debug-frames (replique-debug-test--frames))
            (goto-char (point-min))
            (search-forward "user/g")
            (replique-debug-eval-in-frame)
            (should (equal (list process nil nil nil nil nil '(:thread 41 :frame 2)) opened))
            (should (equal "*replique-debug worker repl*" (buffer-name buffer)))
            (should (eq buffer (replique-debug--frame-repl-buffer process 41)))
            ;; asked again, from another frame: the repl moves there
            (setq opened nil)
            (setf (replique-repl--in-frame repl) '(:thread 41 :frame 2))
            (setq replique-current-repl nil)
            (goto-char (point-min))
            (replique-debug-eval-in-frame)
            (should-not opened)
            (should (equal '("#replique/frame {:thread 41 :frame 0}") directives))
            (should (eq repl replique-current-repl))
            ;; and from the frame it is in already, nothing is said
            (setf (replique-repl--in-frame repl) '(:thread 41 :frame 0))
            (replique-debug-eval-in-frame)
            (should (= 1 (length directives)))))
        (replique-debug--clear process 41)
        (should-not (buffer-live-p buffer))))))

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

(defun replique-debug-test--views (process)
  "Return the views of the locals of thread 41 of PROCESS."
  (replique-debug--views-of process 41))

(ert-deftest replique-debug-test-a-thread-has-one-view-of-locals ()
  "Visiting a frame shows its locals in the view the thread has."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--stop process)
      (with-current-buffer (replique-debug--buffer-of process 41)
        (replique-debug-test--answer :debug-frames (replique-debug-test--frames))
        (goto-char (point-min))
        (search-forward "user/g")
        (replique-debug-visit-frame))
      (let ((views (replique-debug-test--views process)))
        (should (= 1 (length views)))
        (should (equal "*replique-debug worker locals*" (buffer-name (car views))))
        (with-current-buffer (car views)
          (should (equal '(:debug (:thread 41 :frame 2)) replique-inspect--source)))))))

(ert-deftest replique-debug-test-what-is-shown-goes-with-the-thread ()
  "Once the thread goes on: its buffer, its view, and the file opened to show
where it stopped - unless it stops again first, or the file was written in."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (should-not (find-buffer-visiting replique-debug-test--file))
      (replique-debug-test--stop process)
      (let ((source (find-buffer-visiting replique-debug-test--file)))
        (should source)
        (replique-debug--event process '(:tag "event" :event "debug-resumed" :thread 41))
        (should (gethash (cons process 41) replique-debug--leaving))
        ;; stopped again in time: nothing goes
        (replique-debug-test--stop process)
        (should-not (gethash (cons process 41) replique-debug--leaving))
        (replique-debug--event process '(:tag "event" :event "debug-resumed" :thread 41))
        (replique-debug--clear process 41)
        (should-not (replique-debug--buffer-of process 41))
        (should-not (replique-debug-test--views process))
        (should-not (buffer-live-p source)))
      ;; written in, it is somebody's
      (replique-debug-test--stop process)
      (let ((source (find-buffer-visiting replique-debug-test--file)))
        (with-current-buffer source (insert ";; fixed\n") (save-buffer))
        (replique-debug--forgotten process)
        (should-not (replique-debug--buffer-of process 41))
        (should (buffer-live-p source))))))

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

;;; Stepping

(defun replique-debug-test--step-stop (process line column end-line end-column value)
  "Tell PROCESS's editor that thread 41 stopped after a form valued VALUE."
  (replique-debug--event process
                         (list :tag "event" :event "debug-paused" :thread 41
                               :name "worker" :ns "user" :kind "value"
                               :file replique-debug-test--file
                               :line line :column column :end-line end-line
                               :end-column end-column :value value)))

(defun replique-debug-test--after-strings (buffer)
  "Return the values shown in BUFFER, as (TEXT . FACE), in the order of the buffer."
  (with-current-buffer buffer
    (mapcar (lambda (o)
              (let ((text (overlay-get o 'after-string)))
                (cons (substring-no-properties text) (get-text-property 0 'face text))))
            (sort (seq-filter (lambda (o) (overlay-get o 'after-string))
                              (overlays-in (point-min) (point-max)))
                  (lambda (a b) (< (overlay-start a) (overlay-start b)))))))

(defun replique-debug-test--said (buffer form)
  "Return what eldoc says the recorded FORM of BUFFER, its text, evaluated to."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (search-forward form)
      (goto-char (match-beginning 0))
      (when-let* ((text (replique-debug-eldoc)))
        (substring-no-properties text)))))

(ert-deftest replique-debug-test-a-step-shows-the-form-and-its-value ()
  "The form the thread stopped after, and what it evaluated to after it; the
values before it stay, dimmed, and go with the thread."
  (replique-debug-test--with-source
    (replique-debug-test--with
      ;; (inc n), on line 3
      (replique-debug-test--step-stop process 3 11 3 18 "2")
      (let ((source (find-buffer-visiting replique-debug-test--file)))
        (with-current-buffer source
          (should (equal "(inc n)"
                         (buffer-substring-no-properties (overlay-start replique-debug--line)
                                                         (overlay-end replique-debug--line)))))
        (should (equal '((" ⇒ 2" . replique-debug-value))
                       (replique-debug-test--after-strings source)))
        (with-current-buffer (replique-debug--buffer-of process 41)
          (should (string-match-p "⇒ 2" (replique-debug--header))))
        ;; on to the let, which ends on line 5
        (replique-debug--event process '(:tag "event" :event "debug-resumed" :thread 41))
        (replique-debug-test--step-stop process 3 3 5 7 "2")
        (should (equal '((" ⇒ 2" . replique-debug-old-value) (" ⇒ 2" . replique-debug-value))
                       (replique-debug-test--after-strings source)))
        ;; the same form again, as in a loop: its value is the last one
        (replique-debug-test--step-stop process 3 11 3 18 "7")
        (should (equal '((" ⇒ 7" . replique-debug-value) (" ⇒ 2" . replique-debug-old-value))
                       (replique-debug-test--after-strings source)))
        (replique-debug--clear process 41)
        (should-not (and (buffer-live-p source) (replique-debug-test--after-strings source)))))))

(ert-deftest replique-debug-test-a-value-shown-is-one-line-and-short ()
  (let ((replique-debug-value-width 10))
    (should (equal "{:a 1, :b…" (replique-debug--value-text "{:a 1,\n :b 2}")))
    (should (equal "1" (replique-debug--value-text "1")))))

(ert-deftest replique-debug-test-the-steps-name-the-thread ()
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--step-stop process 3 11 3 18 "2")
      (with-current-buffer (replique-debug--buffer-of process 41)
        (replique-debug-next)
        (replique-debug-in)
        (replique-debug-out))
      (should (equal '((:op :debug-step :thread 41 :mode "next")
                       (:op :debug-step :thread 41 :mode "in")
                       (:op :debug-step :thread 41 :mode "out"))
                     (mapcar #'car (seq-filter (lambda (cell)
                                                 (eq :debug-step (plist-get (car cell) :op)))
                                               replique-debug-test--asked)))))))

(ert-deftest replique-debug-test-a-defun-is-sent-to-be-stepped-through ()
  "With the directive asking for it, and the column the form starts at."
  (let ((sent nil))
    (cl-letf (((symbol-function 'replique-repl-ensure-here) (lambda () 'repl))
              ((symbol-function 'replique-repl-send-code)
               (lambda (_repl code &rest _) (setq sent code))))
      (with-temp-buffer
        (replique-clojure-mode)
        (insert "(ns user)\n\n  (defn f [n]\n    (inc n))\n")
        (goto-char (point-min))
        (search-forward "inc")
        (replique-debug-defun)))
    (should (string-match-p
             (regexp-quote "#replique/src {:line 3 :instrument true :column 3}\n(defn f [n]")
             sent))))

(ert-deftest replique-debug-test-the-value-stepped-to-is-browsed ()
  "In a view of its own, which follows the stops - and goes at a stop after
no form."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--step-stop process 3 11 3 18 "2")
      (with-current-buffer (replique-debug--buffer-of process 41)
        (replique-debug-inspect-value))
      (let ((view (replique-debug--view-of process 41 t)))
        (should (equal "*replique-debug worker value*" (buffer-name view)))
        (should (equal '(:debug (:thread 41 :value t))
                       (buffer-local-value 'replique-inspect--source view)))
        ;; and the locals have theirs
        (should-not (eq view (replique-debug--view-of process 41 nil)))
        (replique-debug--event process '(:tag "event" :event "debug-resumed" :thread 41))
        (replique-debug-test--step-stop process 3 3 5 7 "2")
        (should (eq view (replique-debug--view-of process 41 t)))
        (replique-debug--event process '(:tag "event" :event "debug-resumed" :thread 41))
        (replique-debug-test--stop process)
        (should-not (buffer-live-p view)))
      (with-current-buffer (replique-debug--buffer-of process 41)
        (should-error (replique-debug-inspect-value) :type 'user-error)))))

(defun replique-debug-test--instrumented (process unit &optional line)
  "Tell PROCESS's editor that it steps through the defn of the source, as UNIT.

Read from LINE, 2 unless said."
  (let ((line (or line 2)))
    (replique-debug--event process
                           (list :tag "event" :event "debug-instrumented" :unit unit
                                 :ns "user" :file replique-debug-test--file
                                 :line line :column 1 :end-line (+ line 3) :end-column 8))))

(defun replique-debug-test--marks (buffer)
  "Return the units marked in BUFFER, with the text they mark."
  (with-current-buffer buffer
    (mapcar (lambda (o)
              (cons (overlay-get o 'replique-debug-unit)
                    (buffer-substring-no-properties (overlay-start o) (overlay-end o))))
            (replique-debug--marks-in (point-min) (point-max)))))

(ert-deftest replique-debug-test-what-is-stepped-through-is-marked ()
  "Until it is not, as the process says - or as it answers being asked."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (find-file-noselect replique-debug-test--file)))
        (with-current-buffer source (replique-clojure-mode))
        (replique-debug-test--instrumented process 7)
        (should (equal '((7 . "(defn f [n]\n  (let [a (inc n)]\n    (replique.debug/break!)\n    a))"))
                       (replique-debug-test--marks source)))
        (replique-debug--event process '(:tag "event" :event "debug-uninstrumented" :unit 7))
        (should-not (replique-debug-test--marks source))
        (replique-debug-test--instrumented process 8)
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "inc")
          (replique-debug-uninstrument))
        (should (equal '(:op :debug-uninstrument :unit 8)
                       (replique-debug-test--answer :debug-uninstrument
                                                    '(:tag "reply" :uninstrumented (8)))))
        (should-not (replique-debug-test--marks source))
        (with-current-buffer source
          (should-error (replique-debug-uninstrument) :type 'user-error))
        ;; all of them
        (cl-letf (((symbol-function 'replique-process-ensure) (lambda () process)))
          (replique-debug-uninstrument t))
        (should (equal '(:op :debug-uninstrument)
                       (car (replique-debug-test--asked :debug-uninstrument))))))))

(ert-deftest replique-debug-test-what-is-evaluated-again-is-not-stepped-through ()
  "By the process it is evaluated in - and not by another."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (find-file-noselect replique-debug-test--file))
            (other (replique-repl--make :process (replique-process--make :id "other"
                                                                         :directory "/tmp/")))
            (repl (replique-repl--make :process process)))
        (replique-debug-test--instrumented process 7)
        (with-current-buffer source
          (replique-debug--evaluated-again other (point-min) (point-max) nil)
          (should-not (replique-debug-test--asked :debug-uninstrument))
          (replique-debug--evaluated-again repl (point-min) (point-max) nil))
        (should (equal '(:op :debug-uninstrument :unit 7)
                       (replique-debug-test--answer :debug-uninstrument
                                                    '(:tag "reply" :uninstrumented (7)))))
        (should-not (replique-debug-test--marks source))
        ;; and a process that goes takes its marks with it
        (replique-debug-test--instrumented process 9)
        (replique-debug--forgotten process)
        (should-not (replique-debug-test--marks source))))))

(defun replique-debug-test--breaks (buffer)
  "Return the breakpoints of BUFFER, as (TEXT . SITE)."
  (with-current-buffer buffer
    (mapcar (lambda (o)
              (cons (buffer-substring-no-properties (overlay-start o) (overlay-end o))
                    (overlay-get o 'replique-debug-site)))
            (replique-debug--breaks-in (point-min) (point-max)))))

(defmacro replique-debug-test--with-sent (&rest body)
  "Run BODY with the code sent to a repl kept in `sent'."
  (declare (indent 0))
  `(let ((sent nil))
     (cl-letf (((symbol-function 'replique-repl-ensure-here) (lambda () 'repl))
               ((symbol-function 'replique-repl-send-code)
                (lambda (_repl code &rest _) (setq sent code))))
       (ignore sent)
       ,@body)))

(ert-deftest replique-debug-test-a-breakpoint-is-on-the-form-at-point ()
  "Its definition is evaluated to stop at it, and it is set once the process
says it is stepped through: by where it was read from."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--with-sent
        (let ((source (find-file-noselect replique-debug-test--file)))
          (with-current-buffer source
            (replique-clojure-mode)
            (goto-char (point-min))
            (search-forward "inc")
            (replique-debug-break))
          (should (string-match-p
                   (regexp-quote " :line 2 :instrument true :entry false :column 1}\n(defn f [n]")
                   sent))
          (should (equal '(("(inc n)" . nil)) (replique-debug-test--breaks source)))
          (replique-debug-test--instrumented process 7)
          (should (equal '(:op :debug-break :unit 7 :line 3 :column 11)
                         (replique-debug-test--answer :debug-break '(:tag "reply" :site 30))))
          (should (equal '(("(inc n)" . 30)) (replique-debug-test--breaks source)))
          ;; a line above it, written since: the process has it where it was read
          (with-current-buffer source
            (goto-char (point-min))
            (insert "\n"))
          (with-current-buffer source
            (should (eq 'replique-debug-breakpoint
                        (overlay-get (car (replique-debug--breaks-in (point-min) (point-max)))
                                     'face)))
            (goto-char (point-min))
            (search-forward "inc")
            ;; again: taken away
            (replique-debug-break))
          (should (equal '(:op :debug-unbreak :site 30)
                         (replique-debug-test--answer :debug-unbreak '(:tag "reply" :unbroken t))))
          (should-not (replique-debug-test--breaks source))
          ;; with a condition, in a definition stepped through already
          (with-current-buffer source
            (replique-debug-break "(> a 1)"))
          (should (equal '(:op :debug-break :unit 7 :line 3 :column 11 :condition "(> a 1)")
                         (replique-debug-test--answer :debug-break '(:tag "reply" :site 31))))
          ;; not stepped through any more: it stays, to be set again
          (replique-debug--event process '(:tag "event" :event "debug-uninstrumented" :unit 7))
          (should (equal '(("(inc n)" . nil)) (replique-debug-test--breaks source)))
          ;; evaluated again where it is now, a line lower
          (replique-debug-test--instrumented process 8 3)
          (should (equal '(:op :debug-break :unit 8 :line 4 :column 11 :condition "(> a 1)")
                         (car (replique-debug-test--asked :debug-break))))
          ;; refused: there is none
          (replique-debug-test--answer :debug-break
                                       '(:tag "error" :error "no-form" :message "No form"))
          (should-not (replique-debug-test--breaks source)))))))

(ert-deftest replique-debug-test-a-breakpoint-goes-with-the-ordinary-evaluation ()
  (replique-debug-test--with-source
    (replique-debug-test--with
      (replique-debug-test--with-sent
        (let ((source (find-file-noselect replique-debug-test--file))
              (repl (replique-repl--make :process process)))
          (with-current-buffer source
            (replique-clojure-mode)
            (search-forward "inc")
            (replique-debug-break)
            ;; evaluated to be stepped through, it stays
            (replique-debug--evaluated-again repl (point-min) (point-max) t)
            (should (replique-debug-test--breaks source))
            (replique-debug--evaluated-again repl (point-min) (point-max) nil)
            (should-not (replique-debug-test--breaks source))))))))

;;; Recording

(ert-deftest replique-debug-test-a-defun-is-sent-to-be-recorded ()
  (replique-debug-test--with-sent
    (with-temp-buffer
      (replique-clojure-mode)
      (insert "(ns user)\n(defn f [n]\n  (inc n))\n")
      (search-backward "inc")
      (replique-debug-record))
    (should (string-match-p
             (regexp-quote ":line 2 :instrument true :entry false :record true :column 1}")
             sent))))

(defun replique-debug-test--recorded (call values)
  "A reply to `:debug-recorded' about CALL, of the calls 4 and 5, with VALUES."
  (list :tag "reply" :call call
        :calls '((:call 4 :thread "main" :at 0) (:call 5 :thread "worker" :at 0))
        :values values))

(ert-deftest replique-debug-test-what-is-recorded-is-said-on-its-forms ()
  "The values of the last call, as calls come - or of a call gone back to,
which stays - each a value to browse.  Only which call it is is written."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (find-file-noselect replique-debug-test--file))
            (inspected nil))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 5 :end-column 8 :record t
                                     :breaks-only t))
        (should (equal '(:op :debug-recorded :unit 7)
                       (replique-debug-test--answer
                        :debug-recorded
                        (replique-debug-test--recorded
                         5 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18
                                    :value "2" :times 1)
                             (:site 2 :line 3 :column 3 :end-line 5 :end-column 7
                                    :value "2" :times 3))))))
        (let ((shown (replique-debug-test--after-strings source)))
          (should (= 1 (length shown)))
          (should (string-match-p "\\`  ◂ call 2 of 2, the last · worker" (caar shown))))
        (should (equal "⇒ 2 ×3" (replique-debug-test--said source "(let")))
        (should (equal "⇒ 2" (replique-debug-test--said source "(inc n)")))
        ;; a line above it, written since: the forms are where they are now
        (with-current-buffer source
          (goto-char (point-min))
          (insert "\n"))
        ;; another call: the last one is followed
        (replique-debug--event process '(:tag "event" :event "debug-recorded" :unit 7))
        (replique-debug-test--answer
         :debug-recorded
         (replique-debug-test--recorded
          5 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18 :value "5" :times 1))))
        (with-current-buffer source
          (let ((o (seq-find (lambda (o) (overlay-get o 'replique-debug-recorded))
                             (overlays-in (point-min) (point-max)))))
            (should (equal "(inc n)" (buffer-substring-no-properties (overlay-start o)
                                                                     (overlay-end o))))))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "(inc")
          ;; the value at point, of the call shown
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process _keys src &rest _) (setq inspected src))))
            (replique-debug-inspect-recorded)
            (should (equal '(:debug (:site 1 :call 5)) inspected))
            (replique-debug-inspect-recorded t)
            (should (equal '(:debug (:site 1)) inspected)))
          ;; the call before, which stays as calls come
          (replique-debug-recorded-before))
        (should (equal '(:op :debug-recorded :unit 7 :call 4)
                       (replique-debug-test--answer
                        :debug-recorded
                        (replique-debug-test--recorded
                         4 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18
                                    :value "1" :times 1))))))
        (should (seq-find (lambda (s) (string-match-p "◂ call 1 of 2 · main" (car s)))
                          (replique-debug-test--after-strings source)))
        (replique-debug--event process '(:tag "event" :event "debug-recorded" :unit 7))
        (should-not (replique-debug-test--asked :debug-recorded))
        (with-current-buffer source
          (should-error (replique-debug-recorded-before) :type 'user-error)
          (replique-debug-recorded-after))
        (should (equal '(:op :debug-recorded :unit 7) (car (replique-debug-test--asked
                                                            :debug-recorded))))
        ;; let go of, it shows nothing
        (replique-debug--event process '(:tag "event" :event "debug-uninstrumented" :unit 7))
        (should-not (replique-debug-test--after-strings source))))))

(ert-deftest replique-debug-test-a-definition-recorded-has-one-view ()
  "Each value browsed is browsed in the same buffer, named after the
definition, which follows the call shown and goes when the definition
records no more."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (find-file-noselect replique-debug-test--file))
            (shown nil))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 5 :end-column 8 :record t
                                     :breaks-only t))
        (replique-debug-test--answer
         :debug-recorded
         (replique-debug-test--recorded
          5 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18 :value "2" :times 1)
              (:site 2 :line 3 :column 3 :end-line 5 :end-column 7 :value "2" :times 3))))
        (cl-letf (((symbol-function 'replique-inspect-show)
                   (lambda (_process _keys src title buffer &optional quiet)
                     (push (list src title buffer quiet) shown)
                     buffer)))
          (with-current-buffer source
            (goto-char (point-min))
            (search-forward "(inc")
            (replique-debug-inspect-recorded)
            (search-backward "(let")
            (replique-debug-inspect-recorded)
            ;; the call before: the view shows it too, not shown if it was not
            (replique-debug-recorded-before))
          (replique-debug-test--answer
           :debug-recorded
           (replique-debug-test--recorded
            4 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18 :value "1" :times 1))))
          ;; a view of every call stays as it is
          (with-current-buffer source
            (goto-char (point-min))
            (search-forward "(inc")
            (replique-debug-inspect-recorded t)
            (replique-debug-recorded-after))
          (replique-debug-test--answer
           :debug-recorded
           (replique-debug-test--recorded
            5 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18 :value "2" :times 1))))
          ;; and shows the calls that come
          (replique-debug--event process '(:tag "event" :event "debug-recorded" :unit 7))
          (replique-debug-test--answer
           :debug-recorded
           (list :tag "reply" :call 6
                 :calls '((:call 5 :thread "main" :at 0) (:call 6 :thread "main" :at 0))
                 :values '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18
                                  :value "3" :times 1)))))
        (let ((view (nth 2 (car shown))))
          (should (equal "*replique-recorded f*" (buffer-name view)))
          (should (equal '(((:debug (:site 1)) "(inc n), every call of f" t t)
                           ((:debug (:site 1)) "(inc n), every call of f" t nil)
                           ((:debug (:site 2 :call 4)) "(let [a (inc n)] (replique.de…, call 1 of 2" t t)
                           ((:debug (:site 2 :call 5)) "(let [a (inc n)] (replique.de…, call 2 of 2" t nil)
                           ((:debug (:site 1 :call 5)) "(inc n), call 2 of 2" t nil))
                         (mapcar (lambda (s) (list (nth 0 s) (nth 1 s) (eq view (nth 2 s)) (nth 3 s)))
                                 shown)))
          (replique-debug--event process '(:tag "event" :event "debug-uninstrumented" :unit 7))
          (should-not (buffer-live-p view)))))))

(ert-deftest replique-debug-test-a-definition-edited-keeps-what-it-recorded ()
  "What was recorded is of the code as it was sent: once it is edited, the
values of the forms edited go and the others are said as before the edit,
browsed still - and not asked for again.  Edited back, they are as they
were.  The definition is still let go of the ordinary way."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (find-file-noselect replique-debug-test--file))
            (mark nil)
            (inspected nil))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 5 :end-column 8 :record t
                                     :breaks-only t))
        (replique-debug-test--answer
         :debug-recorded
         (replique-debug-test--recorded
          5 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18 :value "2" :times 1)
              (:site 3 :line 5 :column 5 :end-line 5 :end-column 6 :value "2" :times 1))))
        (setq mark (gethash (cons process 7) replique-debug--units))
        (should (equal "⇒ 2" (replique-debug-test--said source "(inc n)")))
        ;; the cursor is put at the end of the line, before which call it is
        (with-current-buffer source
          (should (seq-every-p (lambda (o) (get-text-property 0 'cursor (overlay-get o 'after-string)))
                               (seq-filter (lambda (o) (overlay-get o 'after-string))
                                           (overlays-in (point-min) (point-max))))))
        ;; written above it: the values move with it
        (with-current-buffer source
          (goto-char (point-min))
          (insert ";; above\n"))
        (should (= 1 (length (replique-debug-test--after-strings source))))
        (should (equal "⇒ 2" (replique-debug-test--said source "(inc n)")))
        (should-not (overlay-get mark 'replique-debug-edited))
        ;; written in a form: its value goes, the other's is of before the edit
        (with-current-buffer source
          (search-forward "(inc n")
          (insert " 1"))
        (should (overlay-get mark 'replique-debug-edited))
        (should (overlay-get mark 'before-string))
        (let ((shown (replique-debug-test--after-strings source)))
          (should (= 1 (length shown)))
          (should (string-match-p "◂ call 2 of 2, before the edit" (caar shown))))
        (should-not (replique-debug-test--said source "(inc n 1)"))
        (replique-debug--event process '(:tag "event" :event "debug-recorded" :unit 7))
        (should-not (replique-debug-test--asked :debug-recorded))
        (with-current-buffer source
          (should-error (replique-debug-recorded-before) :type 'user-error)
          ;; the value faded is browsed still, and said as before the edit
          (search-forward "    a")
          (backward-char)
          (should (string-match-p "before the edit" (replique-debug-eldoc)))
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process _keys src &rest _) (setq inspected src))))
            (replique-debug-inspect-recorded))
          (should (equal '(:debug (:site 3 :call 5)) inspected))
          ;; written back to what was recorded: as it was
          (goto-char (point-min))
          (search-forward "(inc n 1")
          (delete-char -2))
        (should-not (overlay-get mark 'replique-debug-edited))
        (should (equal "⇒ 2" (replique-debug-test--said source "(inc n)")))
        (should-not (string-match-p "before the edit"
                                    (caar (replique-debug-test--after-strings source))))
        (with-current-buffer source
          (replique-debug-uninstrument))
        (should (equal '(:op :debug-uninstrument :unit 7)
                       (car (replique-debug-test--asked :debug-uninstrument))))))))

;;; What is written of a recording

(defun replique-debug-test--recording (process)
  "Have PROCESS record the definition of a source with forms on lines of
their own, a literal, and a form over two lines; return its buffer."
  (with-temp-file replique-debug-test--file
    (insert "(ns user)\n(defn f [n]\n  (let [a (inc n) b 10]\n    (+ a\n       b)))\n"))
  (let ((source (find-file-noselect replique-debug-test--file)))
    (replique-debug--event process
                           (list :tag "event" :event "debug-instrumented" :unit 7
                                 :ns "user" :file replique-debug-test--file :line 2
                                 :column 1 :end-line 5 :end-column 12 :record t
                                 :breaks-only t))
    (replique-debug-test--answer
     :debug-recorded
     (replique-debug-test--recorded
      5 '((:site 1 :line 3 :column 11 :end-line 3 :end-column 18 :value "2" :times 1)
          (:site 2 :line 3 :column 16 :end-line 3 :end-column 17 :value "1" :times 1)
          (:site 3 :line 3 :column 21 :end-line 3 :end-column 23 :value "10" :times 1)
          (:site 4 :line 4 :column 8 :end-line 4 :end-column 9 :value "2" :times 1)
          (:site 5 :line 5 :column 8 :end-line 5 :end-column 9 :value "10" :times 1)
          (:site 6 :line 4 :column 5 :end-line 5 :end-column 10 :value "12" :times 1)
          (:site 7 :line 3 :column 3 :end-line 5 :end-column 11 :value "12" :times 3))))
    source))

(ert-deftest replique-debug-test-only-which-call-is-written ()
  "No value is written over the code: which call is shown, at the end of the
first line of the definition, where the cursor stays before it."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (replique-debug-test--recording process)))
        (should (equal '("◂")
                       (mapcar (lambda (s) (if (string-prefix-p "  ◂ call 2 of 2, the last · worker"
                                                                (car s))
                                               "◂"
                                             (car s)))
                               (replique-debug-test--after-strings source))))
        (with-current-buffer source
          (dolist (o (overlays-in (point-min) (point-max)))
            (when-let* ((text (overlay-get o 'after-string)))
              (should (= (overlay-start o) (save-excursion (goto-char (point-min))
                                                           (search-forward "(defn")
                                                           (line-end-position))))
              (should (get-text-property 0 'cursor text)))))))))

(ert-deftest replique-debug-test-eldoc-says-the-value-of-the-form-at-point ()
  "Before what a call takes, whatever is written: the form just written, or
the innermost point is in - highlighted, and the one browsed."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let* ((source (replique-debug-test--recording process))
             (inspected nil))
        (with-current-buffer source
          (should (eq #'replique-debug-eldoc (car eldoc-documentation-functions)))
          (goto-char (point-min))
          (search-forward "(+")
          (should (equal "⇒ 12" (substring-no-properties (replique-debug-eldoc))))
          (search-backward "(inc n)")
          (forward-sexp)
          (should (equal "⇒ 2" (substring-no-properties (replique-debug-eldoc))))
          (replique-debug--follow-point)
          (should (equal "(inc n)" (buffer-substring-no-properties
                                    (overlay-start replique-debug--at-point)
                                    (overlay-end replique-debug--at-point))))
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process _keys src &rest _) (setq inspected src))))
            (replique-debug-inspect-recorded))
          (should (equal '(:debug (:site 1 :call 5)) inspected))
          ;; the literal is not one: the let it is in
          (search-forward "10")
          (backward-char)
          (should (equal "⇒ 12 ×3" (substring-no-properties (replique-debug-eldoc))))
          ;; outside, nothing to say: what a call takes is said
          (goto-char (point-min))
          (should-not (replique-debug-eldoc))
          (replique-debug--follow-point)
          (should-not (overlay-buffer replique-debug--at-point)))))))

;;; What a function was called with

(ert-deftest replique-debug-test-the-args-are-said-on-the-params ()
  "What the fn was called with is the value of its params: said by eldoc,
and browsed, each."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (with-temp-file replique-debug-test--file
        (insert "(ns user)\n(defn f [n & {:keys [k]}]\n  (inc n))\n"))
      (let* ((source (find-file-noselect replique-debug-test--file))
             (inspected nil))
        (with-current-buffer source (replique-clojure-mode))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 3 :end-column 10 :record t
                                     :breaks-only t))
        (replique-debug-test--answer
         :debug-recorded
         (replique-debug-test--recorded
          5 '((:site 1 :line 3 :column 3 :end-line 3 :end-column 10 :value "2" :times 1)
              (:site 2 :kind "enter" :line 2 :column 1 :end-line 3 :end-column 10
                     :value "[1 {:k 2}]" :times 1 :params ("n" "&" "p__1")
                     :args ("1" "(:k 2)")))))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "[n")
          (should (equal "⇒ 1" (substring-no-properties (replique-debug-eldoc))))
          (search-forward "{:keys")
          (should (equal "⇒ (:k 2)" (substring-no-properties (replique-debug-eldoc))))
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process _keys src &rest _) (setq inspected src))))
            (replique-debug-inspect-recorded))
          (should (equal '(:debug (:site 2 :arg 1 :call 5)) inspected)))))))

(ert-deftest replique-debug-test-a-local-is-said-on-its-name ()
  "What a name a loop or a let bound was bound to is a value of its own,
said on the name as a param's is."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (with-temp-file replique-debug-test--file
        (insert "(ns user)\n(defn f [xs]\n  (doseq [x xs] (inc x)))\n"))
      (let ((source (find-file-noselect replique-debug-test--file)))
        (with-current-buffer source (replique-clojure-mode))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 3 :end-column 26 :record t
                                     :breaks-only t))
        (replique-debug-test--answer
         :debug-recorded
         (replique-debug-test--recorded
          5 '((:site 1 :kind "local" :line 3 :column 11 :end-line 3 :end-column 12
                     :value "3" :times 2)
              (:site 2 :line 3 :column 17 :end-line 3 :end-column 24 :value "4" :times 2))))
        (should (equal "⇒ 4 ×2" (replique-debug-test--said source "(inc x)")))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "[x ")
          (backward-char)
          (should (equal "⇒ 3 ×2" (substring-no-properties (replique-debug-eldoc)))))))))

(ert-deftest replique-debug-test-a-loop-is-gone-round-an-iteration-at-a-time ()
  "With point in a loop, what it did in one iteration, the call shown
staying; the iterations kept only, found by what holds in them, and every
one browsed - and the whole call again."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (with-temp-file replique-debug-test--file
        (insert "(ns user)\n(defn f [xs]\n  (doseq [x xs] (inc x)))\n"))
      (let* ((source (find-file-noselect replique-debug-test--file))
             (loops '((:site 1 :line 3 :column 3 :end-line 3 :end-column 25
                             :iterations 25 :kept ((1 9) (16 25)))))
             (answer (lambda (iteration x)
                       (replique-debug-test--answer
                        :debug-recorded
                        (append (list :iteration iteration :loops loops)
                                (replique-debug-test--recorded
                                 5 `((:site 1 :kind "local" :line 3 :column 11 :end-line 3
                                            :end-column 12 :value ,x :times 1)
                                     (:site 2 :line 3 :column 17 :end-line 3 :end-column 24
                                            :value "…" :not-kept t)))))))
             (shown (lambda ()
                      (mapconcat #'car (replique-debug-test--after-strings source) "")))
             (inspected nil))
        (with-current-buffer source (replique-clojure-mode))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 3 :end-column 26 :record t
                                     :breaks-only t))
        (replique-debug-test--answer
         :debug-recorded
         (append (list :loops loops)
                 (replique-debug-test--recorded
                  5 '((:site 1 :kind "local" :line 3 :column 11 :end-line 3 :end-column 12
                             :value "24" :times 25)))))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "(inc")
          (should-error (replique-debug-whole-call) :type 'user-error)
          (replique-debug-next-iteration)
          (should (equal '(:op :debug-recorded :unit 7 :call 5 :iteration (:site 1 :index 1))
                         (funcall answer '(:site 1 :index 1) "0")))
          (should (string-match-p "call 2 of 2 · worker · .* · doseq x, iteration 1 of 25"
                                  (funcall shown)))
          (should (equal "⇒ 0" (replique-debug-test--said source "x xs")))
          (should (equal "⇒ …" (replique-debug-test--said source "(inc x)")))
          (should-error (replique-debug-previous-iteration) :type 'user-error)
          (replique-debug-next-iteration 8)
          (should (equal '(:site 1 :index 9) (plist-get (funcall answer '(:site 1 :index 9) "8")
                                                        :iteration)))
          ;; the iterations not kept are skipped
          (replique-debug-next-iteration)
          (should (equal '(:site 1 :index 16) (plist-get (funcall answer '(:site 1 :index 16) "15")
                                                         :iteration)))
          (replique-debug-find-iteration "(= x 3)")
          (should (equal '(:op :debug-find-iteration :unit 7 :site 1 :line 3 :column 21
                               :code "(= x 3)" :call 5 :from 16)
                         (replique-debug-test--answer :debug-find-iteration
                                                      '(:tag "reply" :index 4))))
          (should (equal '(:site 1 :index 4) (plist-get (funcall answer '(:site 1 :index 4) "3")
                                                        :iteration)))
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process _keys source title buffer &rest _)
                       (setq inspected (list source title))
                       buffer)))
            ;; around the form at point, by iteration
            (replique-debug-inspect-recorded t)
            (should (equal '((:debug (:loop 1 :call 5 :site 2 :only t))
                             "(inc x), every iteration of doseq x, call 2 of 2")
                           inspected))
            (let ((view (overlay-get (replique-debug--recording-at-point) 'replique-debug-view)))
              (with-current-buffer view
                (should replique-debug-view-mode)
                ;; or every name, and the form a column
                (replique-debug-view-locals)
                (should (equal '((:debug (:loop 1 :call 5 :site 2))
                                 "doseq x, every iteration, call 2 of 2")
                               inspected))
                (replique-debug-view-locals)
                ;; wider: every call, by call - with the params or not
                (replique-debug-view-widen)
                (should (equal '((:debug (:site 2)) "(inc x), every call of f") inspected))
                (replique-debug-view-locals)
                (should (equal '((:debug (:calls 7 :site 2)) "f, every call") inspected))
                ;; and back to one value, which has no locals around it
                (replique-debug-view-widen)
                (should (equal '((:debug (:site 2 :call 5 :iteration (:site 1 :index 4)))
                                 "(inc x), call 2 of 2 · doseq x, iteration 4 of 25")
                               inspected))
                (should-error (replique-debug-view-locals) :type 'user-error)))
            ;; on a name, every name
            (search-backward "x xs")
            (replique-debug-inspect-recorded t))
          (should (equal '((:debug (:loop 1 :call 5)) "doseq x, every iteration, call 2 of 2")
                         inspected))
          (replique-debug-whole-call)
          ;; the last call followed again, as it was before
          (should (equal '(:op :debug-recorded :unit 7)
                         (replique-debug-test--answer
                          :debug-recorded
                          (append (list :loops loops) (replique-debug-test--recorded 5 nil)))))
          (should-not (string-match-p "iteration" (funcall shown))))))))

(ert-deftest replique-debug-test-a-fn-is-gone-round-a-call-at-a-time ()
  "A fn the code wrote is a loop whose times round are its calls - named
fn and its params."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (with-temp-file replique-debug-test--file
        (insert "(ns user)\n(defn f [xs]\n  (reduce (fn [acc x] (+ acc x)) 0 xs))\n"))
      (let* ((source (find-file-noselect replique-debug-test--file))
             (loops '((:site 1 :fn t :line 3 :column 11 :end-line 3 :end-column 33
                             :iterations 3 :kept ((1 3)))))
             (sum '(:site 2 :line 3 :column 23 :end-line 3 :end-column 32 :value "1" :times 1))
             (shown (lambda ()
                      (mapconcat #'car (replique-debug-test--after-strings source) "")))
             (inspected nil))
        (with-current-buffer source (replique-clojure-mode))
        (replique-debug--event process
                               (list :tag "event" :event "debug-instrumented" :unit 7
                                     :ns "user" :file replique-debug-test--file :line 2
                                     :column 1 :end-line 3 :end-column 39 :record t
                                     :breaks-only t))
        (replique-debug-test--answer
         :debug-recorded
         (append (list :loops loops) (replique-debug-test--recorded 5 (list sum))))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "(+ acc")
          (replique-debug-next-iteration)
          (replique-debug-test--answer
           :debug-recorded
           (append (list :iteration '(:site 1 :index 1) :loops loops)
                   (replique-debug-test--recorded 5 (list sum))))
          (should (string-match-p "fn \\[acc x\\], call 1 of 3" (funcall shown)))
          (should-error (replique-debug-previous-iteration) :type 'user-error)
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process _keys source title buffer &optional quiet)
                       (setq inspected (list source title quiet))
                       buffer)))
            (replique-debug-inspect-recorded t)
            (should (equal '((:debug (:loop 1 :call 5 :site 2 :only t))
                             "(+ acc x), every call of fn [acc x], call 2 of 2" nil)
                           inspected))
            ;; in the view of the definition, which follows the call shown
            (replique-debug-whole-call)
            (replique-debug-test--answer
             :debug-recorded
             (append (list :loops loops) (replique-debug-test--recorded 5 (list sum))))
            (replique-debug-recorded-before)
            (replique-debug-test--answer
             :debug-recorded
             (append (list :loops loops) (replique-debug-test--recorded 4 (list sum))))
            (should (equal '((:debug (:loop 1 :call 4 :site 2 :only t))
                             "(+ acc x), every call of fn [acc x], call 1 of 2" t)
                           inspected))
            (should (equal "*replique-recorded f*"
                           (buffer-name (overlay-get (replique-debug--recording-at-point)
                                                     'replique-debug-view))))))))))

;;; Recording ClojureScript

(ert-deftest replique-debug-test-clojurescript-is-recorded-and-not-stepped-through ()
  "A .cljs definition is sent to be recorded; to be stepped through, or to
stop at a breakpoint, it is refused before anything is sent."
  (replique-debug-test--with-sent
    (with-temp-buffer
      (replique-clojure-clojurescript-mode)
      (insert "(ns app)\n(defn f [n]\n  (inc n))\n")
      (search-backward "inc")
      (replique-debug-record)
      (should (string-match-p
               (regexp-quote ":line 2 :instrument true :entry false :record true :column 1}")
               sent))
      (setq sent nil)
      (should-error (replique-debug-defun) :type 'user-error)
      (should-error (replique-debug-break) :type 'user-error)
      (should-not sent)
      (should-not (replique-debug--breaks-in (point-min) (point-max))))))

(defun replique-debug-test--cljs-instrumented (process n)
  "Tell PROCESS's editor that it records the definition of the source as N,
in the node program of a ClojureScript repl."
  (replique-debug--event process
                         (list :tag "event" :event "debug-instrumented" :unit n
                               :ns "user" :file replique-debug-test--file :line 2
                               :column 1 :end-line 5 :end-column 8 :record t
                               :breaks-only t :dialect "cljs" :target "node")))

(ert-deftest replique-debug-test-a-clojurescript-unit-is-asked-about-in-its-runtime ()
  "What it recorded is asked of the process by its number, and its values are
browsed in the runtime the event named - and a unit replacing it is said to
be there before it is said to be gone, which is the order the process tells
a ClojureScript one in."
  (replique-debug-test--with-source
    (replique-debug-test--with
      (let ((source (find-file-noselect replique-debug-test--file))
            (inspected nil))
        (replique-debug-test--cljs-instrumented process 8)
        (should (equal '(:op :debug-recorded :unit 8)
                       (replique-debug-test--answer
                        :debug-recorded
                        (list :tag "reply" :call 3
                              :calls '((:call 3 :thread "node" :at 0))
                              :values '((:site 1 :line 3 :column 11 :end-line 3
                                               :end-column 18 :value "2" :times 1))))))
        (should (seq-find (lambda (s) (string-match-p "◂ call 1 of 1, the last · node" (car s)))
                          (replique-debug-test--after-strings source)))
        (with-current-buffer source
          (goto-char (point-min))
          (search-forward "(inc")
          (cl-letf (((symbol-function 'replique-inspect-show)
                     (lambda (_process keys src &rest _) (setq inspected (list keys src)))))
            (replique-debug-inspect-recorded))
          (should (equal '((:dialect :cljs :target :node) (:debug (:site 1 :call 3)))
                         inspected)))
        ;; evaluated to be recorded again: the new unit first
        (replique-debug-test--cljs-instrumented process 9)
        (should (equal '(9) (mapcar #'car (replique-debug-test--marks source))))
        (should-not (seq-remove (lambda (s) (string-match-p "◂" (car s)))
                                (replique-debug-test--after-strings source)))
        (replique-debug--event process '(:tag "event" :event "debug-uninstrumented" :unit 8))
        (should (equal '(9) (mapcar #'car (replique-debug-test--marks source))))
        (should (equal '(:op :debug-recorded :unit 9)
                       (car (replique-debug-test--asked :debug-recorded))))))))

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
                           (lambda () (string-match-p "b 20\\_>" (or (replique-debug-test--locals-text thread) "")))
                           30))
                  (goto-char (point-min))
                  (save-selected-window (replique-debug-eval-in-frame))
                  (let ((in-frame replique-current-repl))
                    (should-not (eq repl in-frame))
                    (should (replique-test-wait-for
                             (lambda () (equal (list :thread thread :frame 0)
                                               (replique-repl--in-frame in-frame)))
                             60))
                    (should (string-match-p "user \\[frame 0\\]=> "
                                            (replique-test-text in-frame)))
                    (should (string-match-p "1022" (replique-test-eval in-frame "(+ a b 1000)")))
                    ;; what a Clojure buffer sends goes there too
                    (with-temp-buffer
                      (replique-clojure-mode)
                      (should (eq in-frame (replique-repl-for-dialect :clj))))
                    (replique-test-eval in-frame "(defn g [x] (* 100 x))"))
                  (replique-debug-restart)
                  (should (replique-test-wait-for
                           (lambda () (string-match-p "b 200\\_>" (or (replique-debug-test--locals-text thread) "")))
                           30))
                  (replique-debug-continue)
                  (should (replique-test-wait-for
                           (lambda () (string-match-p "202" (replique-test-text repl)))
                           30))
                  (should (replique-test-wait-for (lambda () (null replique-debug--pause)) 10))
                  ;; and the repl that evaluated in it goes with the thread
                  (should (replique-test-wait-for
                           (lambda () (null (replique-debug--frame-repl-buffer process thread)))
                           10))
                  (should-not (and replique-current-repl
                                   (replique-repl--in-frame replique-current-repl)))))))
        (when repl
          (replique-conn-close (replique-repl--conn repl))
          (kill-buffer (replique-repl--buffer repl)))
        (replique-kill-process process)))))

(defun replique-debug-test--compiler ()
  "Return the jar of the compiler of replique-clj beside the project, or skip."
  (let ((jar (expand-file-name "../replique-clj/target/clojure-1.12.5-r1.jar"
                               (replique-test-project))))
    (unless (file-exists-p jar)
      (ert-skip (format "No compiler to step with at %s" jar)))
    jar))

(ert-deftest replique-debug-test-a-defun-is-stepped-through ()
  "Two definitions evaluated to be stepped through, the outer one called: it
stops on entering, goes form by form with each value after its form, into
the inner one and out of it, and the repl gets the value."
  (let ((jar (replique-debug-test--compiler)))
    (replique-test-with-project dir
      (with-temp-file (expand-file-name "deps.edn" dir)
        (insert (format "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}}}" jar)))
      (make-directory (expand-file-name "src" dir))
      (let ((file (expand-file-name "src/stepped.clj" dir)))
        (with-temp-file file
          (insert "(ns stepped)\n\n(defn sg [y]\n  (* y 10))\n\n"
                  "(defn sf [m]\n  (let [x (inc (:a m))]\n    (+ x (sg x))))\n"))
        (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
               (replique-debugger nil)
               (process (let ((known replique-processes))
                          (replique-process-start dir nil nil t)
                          (unless (replique-test-wait-for
                                   (lambda () (seq-difference replique-processes known)) 120)
                            (error "The process did not start"))
                          (replique-test--note-process
                           (car (seq-difference replique-processes known)))))
               (repl nil)
               (source nil))
          (unwind-protect
              (save-window-excursion
                (setq repl (replique-repl process))
                (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 60))
                (setq source (find-file-noselect file))
                (with-current-buffer source
                  (replique-eval-defun)       ; the ns
                  (replique-test-settle 1)
                  (goto-char (point-min))
                  (search-forward "(defn sg")
                  (replique-debug-defun)
                  (search-forward "(defn sf")
                  (replique-debug-defun))
                (replique-test-settle 2)
                (should (replique-test-wait-for
                         (lambda () (= 2 (length (replique-debug-test--marks source)))) 30))
                (replique-repl-send-code repl "(stepped/sf {:a 1})")
                (let* ((buffer nil)
                       (kind (lambda ()
                               (and (buffer-live-p buffer)
                                    (plist-get (buffer-local-value 'replique-debug--pause buffer)
                                               :kind)))))
                  (should (replique-test-wait-for
                           (lambda ()
                             (setq buffer (seq-find (lambda (b)
                                                      (with-current-buffer b
                                                        (and (derived-mode-p 'replique-debug-mode)
                                                             replique-debug--pause)))
                                                    (buffer-list))))
                           120))
                  (should (equal "enter" (funcall kind)))
                  (cl-flet ((step (command value)
                              ;; a stop of its own: the one before may have
                              ;; said the same value
                              (let ((before (buffer-local-value 'replique-debug--pause buffer)))
                                (with-current-buffer buffer (funcall command))
                                (should (replique-test-wait-for
                                         (lambda ()
                                           (and (buffer-live-p buffer)
                                                (let ((pause (buffer-local-value
                                                              'replique-debug--pause buffer)))
                                                  (and pause (not (eq pause before))
                                                       (equal value (plist-get pause :value))))))
                                         30)))))
                    (step #'replique-debug-next "1")
                    (step #'replique-debug-next "2")
                    (should (equal '((" ⇒ 1" . replique-debug-old-value)
                                     (" ⇒ 2" . replique-debug-value))
                                   (replique-debug-test--after-strings source)))
                    (let ((before (buffer-local-value 'replique-debug--pause buffer)))
                      (with-current-buffer buffer (replique-debug-in))
                      (should (replique-test-wait-for
                               (lambda () (and (not (eq before (buffer-local-value
                                                               'replique-debug--pause buffer)))
                                               (equal "enter" (funcall kind))))
                               30)))
                    (step #'replique-debug-next "20")
                    (step #'replique-debug-out "20")
                    (with-current-buffer buffer (replique-debug-continue))
                    (should (replique-test-wait-for
                             (lambda () (string-match-p "^22$" (replique-test-text repl)))
                             30))
                    ;; evaluated the ordinary way, sf is not stepped through
                    (with-current-buffer source
                      (goto-char (point-min))
                      (search-forward "(defn sf")
                      (replique-eval-defun))
                    (should (replique-test-wait-for
                             (lambda () (= 1 (length (replique-debug-test--marks source)))) 30))
                    ;; and sg, asked
                    (with-current-buffer source
                      (goto-char (point-min))
                      (search-forward "(defn sg")
                      (replique-debug-uninstrument))
                    (should (replique-test-wait-for
                             (lambda () (null (replique-debug-test--marks source))) 30))
                    ;; a breakpoint: sf is evaluated to stop there, and only there
                    (with-current-buffer source
                      (goto-char (point-min))
                      ;; in (:a m), which is the form point is in
                      (search-forward "(inc (:a")
                      (replique-debug-break))
                    (should (replique-test-wait-for
                             (lambda () (cdar (replique-debug-test--breaks source))) 30))
                    (replique-repl-send-code repl "(stepped/sf {:a 2})")
                    ;; in a buffer of its own, the one before having gone
                    (should (replique-test-wait-for
                             (lambda ()
                               (setq buffer
                                     (seq-find (lambda (b)
                                                 (with-current-buffer b
                                                   (and (derived-mode-p 'replique-debug-mode)
                                                        (plist-get replique-debug--pause :break)
                                                        (equal "2" (plist-get replique-debug--pause
                                                                              :value)))))
                                               (buffer-list))))
                             30))
                    (with-current-buffer buffer (replique-debug-continue))
                    (should (replique-test-wait-for
                             (lambda () (string-match-p "^33$" (replique-test-text repl)))
                             30)))))
            (when (buffer-live-p source) (kill-buffer source))
            (when repl
              (replique-conn-close (replique-repl--conn repl))
              (kill-buffer (replique-repl--buffer repl)))
            (replique-kill-process process)))))))

(ert-deftest replique-debug-test-a-defun-is-recorded ()
  "A definition evaluated to record, called twice in a process started
without the debugger: what its forms evaluated to in the last call is said
on them, the call before is gone back to, and a value browsed."
  (let ((jar (replique-debug-test--compiler)))
    (replique-test-with-project dir
      (with-temp-file (expand-file-name "deps.edn" dir)
        (insert (format "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}}}" jar)))
      (make-directory (expand-file-name "src" dir))
      (let ((file (expand-file-name "src/recorded.clj" dir)))
        (with-temp-file file
          (insert "(ns recorded)\n\n(defn rf [m]\n  (let [x (inc (:a m))]\n"
                  "    (mapv (fn [y] (* x y)) (range 3))))\n"))
        (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
               (replique-debugger nil)
               (process (let ((known replique-processes))
                          (replique-process-start dir nil nil nil)
                          (unless (replique-test-wait-for
                                   (lambda () (seq-difference replique-processes known)) 120)
                            (error "The process did not start"))
                          (replique-test--note-process
                           (car (seq-difference replique-processes known)))))
               (repl nil)
               (source nil)
               (said (lambda (form)
                       (and (buffer-live-p source) (replique-debug-test--marks source)
                            (replique-debug-test--said source form)))))
          (unwind-protect
              (save-window-excursion
                (setq repl (replique-repl process))
                (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 60))
                (setq source (find-file-noselect file))
                (with-current-buffer source
                  (replique-eval-defun)       ; the ns
                  (replique-test-settle 1)
                  (search-forward "(defn rf")
                  (replique-debug-record))
                (should (replique-test-wait-for
                         (lambda () (replique-debug-test--marks source)) 30))
                (replique-repl-send-code repl "(recorded/rf {:a 1})")
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 4 ×3" (funcall said "(* x y)")))
                         30))
                (should (equal "⇒ [0 2 4]" (funcall said "(mapv")))
                (replique-repl-send-code repl "(recorded/rf {:a 2})")
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 6 ×3" (funcall said "(* x y)")))
                         30))
                (with-current-buffer source
                  (goto-char (point-min))
                  (search-forward "(inc (:a")
                  (replique-debug-recorded-before))
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 4 ×3" (funcall said "(* x y)")))
                         30))
                (with-current-buffer source
                  (replique-debug-inspect-recorded))
                (should (replique-test-wait-for
                         (lambda ()
                           (seq-find (lambda (b)
                                       (with-current-buffer b
                                         (and (derived-mode-p 'replique-inspect-mode)
                                              (plist-get (plist-get replique-inspect--source
                                                                    :debug)
                                                         :call)
                                              (string-match-p "\\_<1\\_>"
                                                              (replique-debug-test--text)))))
                                     (buffer-list)))
                         30))
                ;; the last call, followed
                (with-current-buffer source (replique-debug-recorded-after))
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 6 ×3" (funcall said "(* x y)")))
                         30))
                ;; let go of, it shows nothing
                (with-current-buffer source (replique-debug-uninstrument))
                (should (replique-test-wait-for
                         (lambda () (null (replique-debug-test--after-strings source)))
                         30)))
            (when (buffer-live-p source) (kill-buffer source))
            (dolist (b (buffer-list))
              (when (with-current-buffer b (derived-mode-p 'replique-inspect-mode))
                (kill-buffer b)))
            (when repl
              (replique-conn-close (replique-repl--conn repl))
              (kill-buffer (replique-repl--buffer repl)))
            (replique-kill-process process)))))))

(ert-deftest replique-debug-test-a-clojurescript-defun-is-recorded ()
  "A .cljs definition evaluated to record in a node repl, called twice: what
its forms evaluated to in the last call is said on them, the call before
is gone back to, and a value browsed in the node program."
  (let ((jar (replique-debug-test--compiler))
        (cljs (expand-file-name "../replique-cljs" (replique-test-project))))
    (unless (file-directory-p cljs) (ert-skip (format "No ClojureScript compiler at %s" cljs)))
    (unless (executable-find "node") (ert-skip "No node"))
    (replique-test-with-project dir
      (with-temp-file (expand-file-name "deps.edn" dir)
        (insert (format (concat "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}"
                                " replique/replique-cljs {:local/root %S}}}")
                        jar cljs)))
      (make-directory (expand-file-name "src" dir))
      (let ((file (expand-file-name "src/recorded.cljs" dir)))
        (with-temp-file file
          (insert "(ns recorded)\n\n(defn rf [m]\n  (let [x (inc (:a m))]\n"
                  "    (mapv (fn [y] (* x y)) (range 3))))\n"))
        (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
               (replique-debugger nil)
               (process (let ((known replique-processes))
                          (replique-process-start dir nil nil nil)
                          (unless (replique-test-wait-for
                                   (lambda () (seq-difference replique-processes known)) 120)
                            (error "The process did not start"))
                          (replique-test--note-process
                           (car (seq-difference replique-processes known)))))
               (repl nil)
               (source nil)
               (said (lambda (form)
                       (and (buffer-live-p source) (replique-debug-test--marks source)
                            (replique-debug-test--said source form)))))
          (unwind-protect
              (save-window-excursion
                (setq repl (replique-repl process :cljs :node))
                (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 120))
                (setq source (find-file-noselect file))
                (with-current-buffer source
                  (replique-eval-defun)       ; the ns
                  (replique-test-settle 2)
                  (search-forward "(defn rf")
                  (replique-debug-record))
                (should (replique-test-wait-for
                         (lambda () (replique-debug-test--marks source)) 60))
                (replique-repl-send-code repl "(recorded/rf {:a 1})")
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 4 ×3" (funcall said "(* x y)")))
                         30))
                (should (equal "⇒ [0 2 4]" (funcall said "(mapv")))
                (should (seq-find (lambda (s) (string-match-p "· node" (car s)))
                                  (replique-debug-test--after-strings source)))
                (replique-repl-send-code repl "(recorded/rf {:a 2})")
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 6 ×3" (funcall said "(* x y)")))
                         30))
                (with-current-buffer source
                  (goto-char (point-min))
                  (search-forward "(inc (:a")
                  (replique-debug-recorded-before))
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 4 ×3" (funcall said "(* x y)")))
                         30))
                (with-current-buffer source
                  (replique-debug-inspect-recorded))
                (should (replique-test-wait-for
                         (lambda ()
                           (seq-find (lambda (b)
                                       (with-current-buffer b
                                         (and (derived-mode-p 'replique-inspect-mode)
                                              (equal :cljs (plist-get replique-inspect--keys
                                                                      :dialect))
                                              (string-match-p "\\_<1\\_>"
                                                              (replique-debug-test--text)))))
                                     (buffer-list)))
                         30))
                ;; evaluated again the ordinary way, it is let go of
                (with-current-buffer source
                  (goto-char (point-min))
                  (search-forward "(defn rf")
                  (replique-eval-defun))
                (should (replique-test-wait-for
                         (lambda () (and (null (replique-debug-test--marks source))
                                         (null (replique-debug-test--after-strings source))))
                         30)))
            (when (buffer-live-p source) (kill-buffer source))
            (dolist (b (buffer-list))
              (when (with-current-buffer b (derived-mode-p 'replique-inspect-mode))
                (kill-buffer b)))
            (when repl
              (replique-conn-close (replique-repl--conn repl))
              (kill-buffer (replique-repl--buffer repl)))
            (replique-kill-process process)))))))

(ert-deftest replique-debug-test-a-clojurescript-loop-is-gone-round ()
  "A loop of a .cljs definition recorded in a node repl, an iteration at a
time: what its code evaluated then is said, and an iteration is found by
what holds in it."
  (let ((jar (replique-debug-test--compiler))
        (cljs (expand-file-name "../replique-cljs" (replique-test-project))))
    (unless (file-directory-p cljs) (ert-skip (format "No ClojureScript compiler at %s" cljs)))
    (unless (executable-find "node") (ert-skip "No node"))
    (replique-test-with-project dir
      (with-temp-file (expand-file-name "deps.edn" dir)
        (insert (format (concat "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}"
                                " replique/replique-cljs {:local/root %S}}}")
                        jar cljs)))
      (make-directory (expand-file-name "src" dir))
      (let ((file (expand-file-name "src/iterated.cljs" dir)))
        (with-temp-file file
          (insert "(ns iterated)\n\n(defn il [xs]\n  (doseq [x xs]\n    (inc x)))\n"))
        (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
               (replique-debugger nil)
               (process (let ((known replique-processes))
                          (replique-process-start dir nil nil nil)
                          (unless (replique-test-wait-for
                                   (lambda () (seq-difference replique-processes known)) 120)
                            (error "The process did not start"))
                          (replique-test--note-process
                           (car (seq-difference replique-processes known)))))
               (repl nil)
               (source nil)
               (shown (lambda ()
                        (mapconcat #'car (replique-debug-test--after-strings source) "")))
               (said (lambda ()
                       (and (replique-debug-test--marks source)
                            (replique-debug-test--said source "(inc x)")))))
          (unwind-protect
              (save-window-excursion
                (setq repl (replique-repl process :cljs :node))
                (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 120))
                (setq source (find-file-noselect file))
                (with-current-buffer source
                  (replique-eval-defun)       ; the ns
                  (replique-test-settle 2)
                  (search-forward "(defn il")
                  (replique-debug-record))
                (should (replique-test-wait-for
                         (lambda () (replique-debug-test--marks source)) 60))
                (replique-repl-send-code repl "(iterated/il [1 2 3])")
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 4 ×3" (funcall said)))
                         30))
                (with-current-buffer source
                  (goto-char (point-min))
                  (search-forward "(inc")
                  (replique-debug-next-iteration))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "doseq x, iteration 1 of 3" (funcall shown)))
                         30))
                (should (equal "⇒ 2" (funcall said)))
                (with-current-buffer source (replique-debug-next-iteration))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "iteration 2 of 3" (funcall shown)))
                         30))
                (should (equal "⇒ 3" (funcall said)))
                ;; found by what holds in it
                (with-current-buffer source (replique-debug-find-iteration "(= x 3)"))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "iteration 3 of 3" (funcall shown)))
                         60))
                (with-current-buffer source (replique-debug-whole-call))
                (should (replique-test-wait-for
                         (lambda () (not (string-match-p "iteration" (funcall shown))))
                         30))
                (should (equal "⇒ 4 ×3" (funcall said))))
            (when (buffer-live-p source) (kill-buffer source))
            (when repl
              (replique-conn-close (replique-repl--conn repl))
              (kill-buffer (replique-repl--buffer repl)))
            (replique-kill-process process)))))))

(provide 'replique-debug-test)

;;; replique-debug-test.el ends here
