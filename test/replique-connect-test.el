;;; replique-connect-test.el --- Tests for choosing, stopping and restarting a process  -*- lexical-binding: t; -*-

;;; Commentary:

;; `replique-connect' is the one way to a process: one Emacs has, one a port
;; file names, or one to start.  What it offers is tested here without a
;; process - a port file and a listener are what it reads.  So are the
;; commands that open a repl on a process there is not yet, which choose one
;; first, and what stopping a process does to its buffers.  Restarting is
;; tested with stand-ins for the stop and the start; a restart of a real
;; process is in `replique-test'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-repl-choice-test)
(require 'replique-repl)

(defun replique-connect-test--labels ()
  "Return the labels `replique-connect' offers, in order."
  (mapcar #'car (replique-repl--process-choices)))

(defmacro replique-connect-test--in-project (name &rest body)
  "Run BODY in a project NAME, with a deps.edn and no process anywhere."
  (declare (indent 1))
  `(replique-test-with-project ,name
     (with-temp-file (expand-file-name "deps.edn" ,name) (insert "{}"))
     (let ((replique-processes nil)
           (replique-current-process nil))
       (replique-test-in-directory ,name ,@body))))

(ert-deftest replique-connect-test-a-start-is-offered-where-nothing-runs ()
  (replique-connect-test--in-project dir
    (should (equal (list (format "Start a process in %s" (abbreviate-file-name dir))
                         "Start or connect in another directory...")
                   (replique-connect-test--labels)))))

(ert-deftest replique-connect-test-a-running-process-is-offered-instead-of-a-start ()
  "A directory has one process, so a port file that something answers for
is what there is to connect to - and a start there would be refused."
  (let ((listener (replique-test-listener 'silent)))
    (unwind-protect
        (replique-connect-test--in-project dir
          (replique-test-write-port-file
           dir (list :process-id "running" :host "127.0.0.1"
                     :port (process-contact listener :service)
                     :pid 1 :started-at 1))
          (let ((choices (replique-repl--process-choices)))
            (should (eq 'connect (nth 1 (car choices))))
            (should (string-prefix-p "running (127.0.0.1:" (caar choices)))
            (should-not (seq-find (lambda (choice) (eq 'start (nth 1 choice)))
                                  choices))))
      (delete-process listener))))

(ert-deftest replique-connect-test-a-port-file-nothing-answers-is-not-offered ()
  (replique-connect-test--in-project dir
    (let ((file (replique-test-write-port-file
                 dir (list :process-id "gone" :host "127.0.0.1"
                           :port (replique-test-free-port)
                           :pid 1 :started-at 1))))
      (should (string-prefix-p "Start a process in" (car (replique-connect-test--labels))))
      (should-not (file-exists-p file)))))

(ert-deftest replique-connect-test-the-current-process-comes-first ()
  (replique-repl-choice-test--with-repls ()
    (let* ((other (replique-repl-choice-test--process "other"))
           (replique-processes (list other process)))
      (replique-connect-test--in-project dir
        (let ((replique-processes (list other process))
              (replique-current-process process))
          (let ((choices (replique-repl--process-choices)))
            (should (equal (list 'process process) (cdr (nth 0 choices))))
            (should (equal (list 'process other) (cdr (nth 1 choices))))))))))

(ert-deftest replique-connect-test-a-process-emacs-has-is-answered-at-once ()
  (replique-repl-choice-test--with-repls ()
    (let ((replique-current-process nil)
          (answered nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) (replique-repl--process-label process))))
        (replique-repl--choose-process (lambda (p) (setq answered p))))
      (should (eq process answered))
      (should (eq process replique-current-process)))))

(ert-deftest replique-connect-test-connect-shows-the-repl-a-process-has ()
  (replique-repl-choice-test--with-repls ((_older nil nil) (newer nil nil))
    (let ((shown nil))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq shown buffer)))
                ((symbol-function 'replique-repl--open)
                 (lambda (&rest _) (error "Opened a repl"))))
        (replique-repl--show process))
      (should (eq (replique-repl--buffer newer) shown))
      (should (eq newer replique-current-repl)))))

(ert-deftest replique-connect-test-connect-opens-a-clojure-repl-where-there-is-none ()
  (replique-repl-choice-test--with-repls ()
    (let ((opened nil))
      (cl-letf (((symbol-function 'replique-repl--open)
                 (lambda (&rest args) (setq opened args))))
        (replique-repl--show process))
      (should (equal (list process nil nil nil) opened)))))

;;; Opening a repl on a process there is not yet

(defmacro replique-connect-test--without-a-process (process opened &rest body)
  "Run BODY with no process, choosing one answering PROCESS.

OPENED is bound to the arguments `replique-repl--open' was called with."
  (declare (indent 2))
  `(let ((replique-processes nil)
         (replique-current-process nil)
         (,opened nil))
     (cl-letf (((symbol-function 'replique-repl--choose-process)
                (lambda (then) (funcall then ,process)))
               ((symbol-function 'replique-repl--open)
                (lambda (&rest args) (setq ,opened args))))
       ,@body)))

(ert-deftest replique-connect-test-a-clojurescript-repl-asks-for-its-namespace-once-there-is-a-process ()
  "The target is asked first, there being nothing to ask a process about,
and the namespace once the process is there: what is offered is what that
process knows of."
  (let* ((process (replique-process--make :id "new"))
         (asked-main-of nil))
    (replique-connect-test--without-a-process process opened
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _)
                   (if (string-match-p "Target" prompt) "node"
                     (error "Asked for %s" prompt))))
                ((symbol-function 'replique-repl--read-main)
                 (lambda (p) (setq asked-main-of p) "my.app")))
        (call-interactively #'replique-cljs))
      (should (eq process asked-main-of))
      (should (equal (list process :cljs :node "my.app") opened)))))

(ert-deftest replique-connect-test-a-clojurescript-repl-on-the-current-process ()
  (replique-repl-choice-test--with-repls ()
    (let ((opened nil))
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "browser"))
                ((symbol-function 'replique-repl--read-main) (lambda (_) nil))
                ((symbol-function 'replique-repl--choose-process)
                 (lambda (_) (error "Asked for a process")))
                ((symbol-function 'replique-repl--open)
                 (lambda (&rest args) (setq opened args))))
        (call-interactively #'replique-cljs))
      (should (equal (list process :cljs :browser nil) opened)))))

(ert-deftest replique-connect-test-a-repl-with-no-process-chooses-one ()
  (let ((process (replique-process--make :id "new")))
    (replique-connect-test--without-a-process process opened
      (call-interactively #'replique-repl)
      (should (equal (list process nil nil nil) opened)))))

(ert-deftest replique-connect-test-a-prefix-with-no-process-asks-once-there-is-one ()
  (let ((process (replique-process--make :id "new"))
        (asked-of nil))
    (replique-connect-test--without-a-process process opened
      (cl-letf (((symbol-function 'replique-repl--read-for)
                 (lambda (p) (setq asked-of p) (list :cljs :node "my.app"))))
        (let ((current-prefix-arg '(4)))
          (call-interactively #'replique-repl)))
      (should (eq process asked-of))
      (should (equal (list process :cljs :node "my.app") opened)))))

;;; The buffers of a process that is let go of

(defun replique-connect-test--in-repl-mode (repl)
  "Make the buffer of REPL a repl buffer, the way opening it does."
  (with-current-buffer (replique-repl--buffer repl)
    (replique-repl-mode)
    (setq-local replique--buffer-repl repl)))

(ert-deftest replique-connect-test-letting-go-of-a-process-kills-its-buffers ()
  "Every repl buffer of it, the ones already closed with them, and its
output - and nothing of another process."
  (replique-repl-choice-test--with-repls ((open nil nil) (quit nil nil))
    (let* ((other (replique-repl-choice-test--process "other"))
           (elsewhere (replique-repl-choice-test--repl other))
           (output (replique-process-buffer process)))
      (mapc #'replique-connect-test--in-repl-mode (list open quit elsewhere))
      ;; quit: closed, and so no longer one of its repls
      (setf (replique-process--repls process) (list open))
      (replique-disconnect process)
      (should-not (buffer-live-p (replique-repl--buffer open)))
      (should-not (buffer-live-p (replique-repl--buffer quit)))
      (should-not (buffer-live-p output))
      (should (buffer-live-p (replique-repl--buffer elsewhere))))))

(ert-deftest replique-connect-test-a-process-that-went-leaves-its-buffers ()
  (replique-repl-choice-test--with-repls ((repl nil nil))
    (replique-connect-test--in-repl-mode repl)
    (replique-process--close process)
    (should (buffer-live-p (replique-repl--buffer repl)))))

;;; Restarting

(ert-deftest replique-connect-test-a-restart-opens-the-repls-again-where-they-were ()
  "Oldest first, in their own buffers, as what they were, printing the way
they were - and the one that was current is current again."
  (replique-repl-choice-test--with-repls ((clj nil nil) (cljs "cljs" "node"))
    (setf (replique-repl--main cljs) "my.app")
    (setf (replique-repl--params clj) '(:print-length 5 :print-level nil))
    (setq replique-current-repl clj)
    (let* ((new (replique-process--make :id "choice-test" :repls nil))
           (started-in nil)
           (forced nil)
           (opened nil))
      (cl-letf (((symbol-function 'replique-process--shut) (lambda (&rest _) t))
                ((symbol-function 'replique-process-start)
                 (lambda (directory then &optional force)
                   (setq started-in directory forced force)
                   (funcall then new)))
                ((symbol-function 'replique-repl--open)
                 (lambda (process dialect target main buffer params)
                   (let ((repl (replique-repl--make :process process :buffer buffer)))
                     (push (list dialect target main buffer params) opened)
                     (push repl (replique-process--repls process))
                     (setq replique-current-repl repl)
                     repl))))
        (replique-restart process))
      (should (equal "/tmp/" started-in))
      ;; With the classpath computed again rather than read from what the
      ;; cli kept, since a changed classpath is what a restart is usually for
      (should forced)
      (should (equal (list (list nil nil nil (replique-repl--buffer clj)
                                 '(:print-length 5 :print-level nil))
                           (list :cljs :node "my.app" (replique-repl--buffer cljs) nil))
                     (reverse opened)))
      (should (eq (replique-repl--buffer clj)
                  (replique-repl--buffer replique-current-repl)))
      (should (eq replique-current-repl (car (replique-process--repls new)))))))

(ert-deftest replique-connect-test-a-process-that-would-not-stop-is-not-started-again ()
  (replique-repl-choice-test--with-repls ((_repl nil nil))
    (cl-letf (((symbol-function 'replique-process--shut) (lambda (&rest _) nil))
              ((symbol-function 'replique-process-start)
               (lambda (&rest _) (error "Started"))))
      (should-error (replique-restart process) :type 'user-error))))

(ert-deftest replique-connect-test-stopping-asks-which-process-current-first ()
  "Asked even when there is one: the commands that read a process this way
stop it or let go of it.  The current one is the default, and RET takes it."
  (replique-repl-choice-test--with-repls ()
    (let* ((other (replique-repl-choice-test--process "other"))
           (replique-processes (list other process))
           (asked nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest args)
                   (setq asked (cons (mapcar #'car collection) (nth 4 args)))
                   (nth 4 args))))
        (should (eq process (replique-repl--read-process "Kill process: "))))
      (should (equal (list (replique-repl--process-label process)
                           (replique-repl--process-label other))
                     (car asked)))
      (should (equal (replique-repl--process-label process) (cdr asked))))))

(ert-deftest replique-connect-test-stopping-with-no-process-says-so ()
  (let ((replique-processes nil)
        (replique-current-process nil))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (error "Asked"))))
      (should-error (replique-repl--read-process "Kill process: ")
                    :type 'user-error))))

(provide 'replique-connect-test)

;;; replique-connect-test.el ends here
