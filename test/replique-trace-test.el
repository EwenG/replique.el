;;; replique-trace-test.el --- Tests for the calls of functions traced  -*- lexical-binding: t; -*-

;;; Commentary:

;; Which functions are traced, and the tree of their calls, without a
;; process: the requests are kept and answered by the test.  At the bottom,
;; one with a process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-trace)

(defvar replique-trace-test--asked nil
  "The requests not answered yet, oldest first: (MSG . CALLBACK).")

(defmacro replique-trace-test--with (&rest body)
  "Run BODY with a process that keeps what it is asked, to be answered."
  (declare (indent 0))
  `(let ((replique-trace-test--asked nil)
         (process (replique-process--make :id "trace-test" :directory "/tmp/")))
     (ignore process)
     (cl-letf (((symbol-function 'replique-process-live-p) (lambda (p) (and p t)))
               ((symbol-function 'replique-process-ensure) (lambda () process))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional callback)
                  (setq replique-trace-test--asked
                        (append replique-trace-test--asked (list (cons msg callback))))
                  nil)))
       (unwind-protect (save-window-excursion ,@body)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'replique-trace-mode)
               (let ((kill-buffer-hook nil)) (kill-buffer buffer)))))))))

(defun replique-trace-test--answer (op frame)
  "Answer the oldest request for OP with FRAME, and return what it asked."
  (let ((cell (or (seq-find (lambda (cell) (eq op (plist-get (car cell) :op)))
                            replique-trace-test--asked)
                  (error "Nothing asked %s" op))))
    (setq replique-trace-test--asked (delq cell replique-trace-test--asked))
    (when (cdr cell) (funcall (cdr cell) frame))
    (car cell)))

(defun replique-trace-test--text (buffer)
  "Return what BUFFER shows."
  (with-current-buffer buffer (buffer-substring-no-properties (point-min) (point-max))))

;;; Which functions

(ert-deftest replique-trace-test-a-diff-says-which-lines-it-touches ()
  (should (equal '(("src/a.clj" (3 . 4) (10 . 10)) ("src/b.cljs" (1 . 1)))
                 (replique-trace--hunks
                  (concat "diff --git a/src/a.clj b/src/a.clj\n"
                          "--- a/src/a.clj\n+++ b/src/a.clj\n"
                          "@@ -3 +3,2 @@ (defn f\n-x\n+y\n+z\n"
                          "@@ -12,2 +10,0 @@\n-a\n-b\n"
                          "diff --git a/src/gone.clj b/src/gone.clj\n"
                          "--- a/src/gone.clj\n+++ /dev/null\n@@ -1 +0,0 @@\n-x\n"
                          "--- a/src/b.cljs\n+++ b/src/b.cljs\n@@ -1 +1 @@\n-x\n+y\n")))))

(ert-deftest replique-trace-test-the-functions-a-diff-touches ()
  "The definitions of functions whose lines are touched, by their names in the
namespace of the file - not a macro, nor what is not a definition."
  (let ((file (make-temp-file "replique-trace-test" nil ".clj")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "(ns app.core)\n\n(defn a [x]\n  x)\n\n(defmacro m [x] x)\n\n"
                    "(defn- ^:private b [] 1)\n(def c (atom 0))\n(comment (a 1))\n"))
          (should (equal '("app.core/a") (replique-trace--touched file '((4 . 4)))))
          (should (equal '("app.core/b") (replique-trace--touched file '((6 . 8)))))
          (should (equal '("app.core/a" "app.core/b" "app.core/c")
                         (replique-trace--touched file t))))
      (delete-file file))))

(ert-deftest replique-trace-test-the-functions-a-branch-changed ()
  "Against a revision: what changed since, committed or not, and new files."
  (unless (executable-find "git") (ert-skip "No git"))
  (let ((root (file-name-as-directory (make-temp-file "replique-trace-git" t))))
    (unwind-protect
        (cl-flet ((git (&rest args) (apply #'replique-trace--git root args))
                  (write (name text)
                    (let ((f (expand-file-name name root)))
                      (make-directory (file-name-directory f) t)
                      (with-temp-file f (insert text)))))
          (git "init" "-q")
          (git "config" "user.email" "t@t")
          (git "config" "user.name" "t")
          (write "src/a.clj" "(ns a)\n(defn f [] 1)\n(defn g [] 2)\n")
          (write "src/b.cljc" "(ns b)\n(defn h [] 3)\n")
          (git "add" ".")
          (git "commit" "-q" "-m" "base")
          (write "src/a.clj" "(ns a)\n(defn f [] 1)\n(defn g [] 20)\n")
          (write "src/b.cljc" "(ns b)\n(defn h [] 30)\n")
          (write "src/c.cljs" "(ns c)\n(defn k [] 4)\n")
          (should (equal '((:clj "a/g" "b/h") (:cljs "b/h" "c/k"))
                         (replique-trace--changed root "HEAD"))))
      (delete-directory root t))))

(ert-deftest replique-trace-test-what-is-traced-is-asked-for ()
  "The function at point, it and what is around it, its namespace - each
after what changed on disk was offered to be loaded."
  (replique-trace-test--with
    (cl-letf* ((offered 0)
               ((symbol-function 'replique-fresh-ensure)
                (lambda (what)
                  (should (equal "tracing" what))
                  (should-not (assq :trace (mapcar (lambda (c) (cons (plist-get (car c) :op) c))
                                                    replique-trace-test--asked)))
                  (setq offered (1+ offered)))))
     (with-temp-buffer
      (replique-clojure-mode)
      (insert "(ns app.core)\n(defn f [n]\n  (inc n))\n")
      (search-backward "inc")
      (replique-trace ?f)
      (should (equal '(:op :trace :vars ["app.core/f"])
                     (replique-trace-test--answer :trace '(:tag "reply" :traced ("app.core/f")))))
      (replique-trace ?a 2)
      (should (equal '(:op :trace :around "app.core/f" :depth 2)
                     (replique-trace-test--answer :trace '(:tag "reply" :traced ("app.core/f")))))
      (replique-trace ?n)
      (should (equal '(:op :trace :ns "app.core")
                     (replique-trace-test--answer :trace '(:tag "reply" :traced ("app.core/f")))))
      (should (= 3 offered))))
    (should (replique-trace--buffer-of process nil))))

(ert-deftest replique-trace-test-a-diff-is-traced-in-the-dialect-of-the-buffer ()
  "A .clj buffer traces what changed of Clojure, a .cljs one of ClojureScript,
a .cljc one of the dialect of the repl in hand."
  (replique-trace-test--with
    (cl-letf* ((repl-dialect :clj)
               ((symbol-function 'replique-fresh-ensure) #'ignore)
               ((symbol-function 'locate-dominating-file) (lambda (&rest _) "/tmp/"))
               ((symbol-function 'replique-trace--base) (lambda (_) "master"))
               ((symbol-function 'read-string) (lambda (_prompt initial) initial))
               ((symbol-function 'replique-repl-for-dialect) #'ignore)
               ((symbol-function 'replique-dialect--of-current-repl) (lambda () repl-dialect))
               ((symbol-function 'replique-trace--changed)
                (lambda (_root _base) '((:clj "a/g" "b/h") (:cljs "b/h" "c/k"))))
               (traced (lambda (mode)
                         (with-temp-buffer
                           (funcall mode)
                           (replique-trace ?d))
                         (prog1 (mapcar #'car replique-trace-test--asked)
                           (setq replique-trace-test--asked nil)))))
      (should (equal '((:op :trace :vars ["a/g" "b/h"]))
                     (funcall traced #'replique-clojure-mode)))
      (should (equal '((:op :trace :vars ["b/h" "c/k"] :dialect :cljs))
                     (funcall traced #'replique-clojure-clojurescript-mode)))
      (should (equal '((:op :trace :vars ["a/g" "b/h"]))
                     (funcall traced #'replique-clojure-clojurec-mode)))
      (setq repl-dialect :cljs)
      (should (equal '((:op :trace :vars ["b/h" "c/k"] :dialect :cljs))
                     (funcall traced #'replique-clojure-clojurec-mode))))))

(ert-deftest replique-trace-test-what-is-not-traced-is-said ()
  "How many were traced, and why the others were not, by reason."
  (let ((said (lambda (frame)
                (let ((text nil))
                  (cl-letf (((symbol-function 'message)
                             (lambda (fmt &rest args) (setq text (apply #'format fmt args)))))
                    (replique-trace--said frame "6 functions changed since 57711eb"))
                  text))))
    (should (string-prefix-p "replique: 6 functions changed since 57711eb: 6 traced - "
                             (funcall said '(:tag "reply" :asked 6 :traced ("a/f")))))
    (should (string-prefix-p
             (concat "replique: 6 functions changed since 57711eb: 1 traced, 5 not - "
                     "no such var, its file not loaded? (4): a/f, a/g, a/h, …; "
                     "not a function (1): a/x - ")
             (funcall said '(:tag "reply" :asked 6
                                  :skipped ((:var "a/f" :why "no such var")
                                            (:var "a/x" :why "not a function")
                                            (:var "a/g" :why "no such var")
                                            (:var "a/h" :why "no such var")
                                            (:var "a/i" :why "no such var"))))))))

;;; The calls

(defun replique-trace-test--calls ()
  "The calls made in none, as the process answers them."
  '(:tag "reply" :traced ("app/handle" "app/f" "app/boom") :kept 4 :dropped 0
    :calls ((:call 1 :var "app/handle" :args ("{:n 1}") :state "returned" :value "13"
                   :made 3 :kept 2 :thread "main" :ms 1.25)
            (:call 9 :var "app/boom" :args () :state "threw"
                   :thrown "clojure.lang.ExceptionInfo: none" :made 0 :kept 0 :thread "main"))))

(ert-deftest replique-trace-test-the-calls-are-a-tree ()
  "Each with what it was called with and returned, opened on the calls it
made; what it returned and was called with browsed."
  (replique-trace-test--with
    (let ((buffer (replique-trace--show process nil))
          (inspected nil))
      (should (equal '(:op :trace-calls)
                     (replique-trace-test--answer :trace-calls (replique-trace-test--calls))))
      (let ((text (replique-trace-test--text buffer)))
        (should (string-match-p "^3 functions traced · 4 calls kept" text))
        (should (string-match-p "^▸ (app/handle {:n 1}) ⇒ 13  · 1.2 ms · main · 3 calls$" text))
        (should (string-match-p "^  (app/boom) ⇒ clojure.lang.ExceptionInfo: none  · main$" text)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "handle")
        (replique-trace-toggle)
        (replique-trace-test--answer :trace-calls (replique-trace-test--calls))
        (should (equal '(:op :trace-calls :parent 1)
                       (replique-trace-test--answer
                        :trace-calls
                        '(:tag "reply" :calls ((:call 2 :var "app/f" :args ("1") :state "returned"
                                                      :value "2" :made 0 :kept 0 :parent 1)
                                               (:call 3 :var "app/f" :args ("10") :state "running"
                                                      :made 0 :kept 0 :parent 1))))))
        (let ((text (replique-trace-test--text buffer)))
          (should (string-match-p "^▾ (app/handle" text))
          (should (string-match-p "^    (app/f 1) ⇒ 2" text))
          (should (string-match-p "^    (app/f 10) …" text))
          (should (string-match-p "… 1 call not kept" text)))
        ;; point stays on the call it was on
        (should (string-match-p "handle" (buffer-substring (line-beginning-position)
                                                           (line-end-position))))
        (cl-letf (((symbol-function 'replique-inspect-show)
                   (lambda (_process _keys src &rest _) (setq inspected src))))
          (replique-trace-inspect-value)
          (should (equal '(:trace (:call 1 :value t)) inspected))
          (replique-trace-inspect-args)
          (should (equal '(:trace (:call 1 :args t)) inspected)))
        ;; cleared, asked again
        (replique-trace-clear)
        (replique-trace-test--answer :trace-clear '(:tag "reply" :cleared 4))
        (should (equal '(:op :trace-calls) (car (car replique-trace-test--asked))))))))

(ert-deftest replique-trace-test-the-tree-does-not-jump ()
  "Written again, the window shows the call at point on the line it was."
  (replique-trace-test--with
    (let ((buffer (replique-trace--show process nil))
          (calls (cl-loop for n from 1 to 60
                          collect (list :call n :var "app/f" :args (list (number-to-string n))
                                        :state "returned" :value "1"
                                        :made (if (= n 30) 1 0) :kept (if (= n 30) 1 0)))))
      (replique-trace-test--answer
       :trace-calls (list :tag "reply" :traced ["app/f"] :kept 60 :calls calls))
      (switch-to-buffer buffer)
      (goto-char (point-min))
      (forward-line 25)
      (set-window-start nil (point))
      (search-forward "(app/f 30)")
      (replique-trace-toggle)
      (replique-trace-test--answer
       :trace-calls (list :tag "reply" :traced ["app/f"] :kept 61 :calls calls))
      (replique-trace-test--answer
       :trace-calls '(:tag "reply" :calls ((:call 61 :var "app/g" :args ("2") :state "returned"
                                                  :value "3" :made 0 :kept 0 :parent 30))))
      (should (string-match-p "^▾ (app/f 30)" (buffer-substring-no-properties
                                               (line-beginning-position) (line-end-position))))
      (should (= 26 (line-number-at-pos (window-start))))
      (should (= (point) (window-point))))))

(ert-deftest replique-trace-test-tracing-is-stopped ()
  "The calls stay, or with a prefix argument are forgotten with the buffer."
  (replique-trace-test--with
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t)))
      (let ((buffer (replique-trace--show process nil)))
        (replique-trace-test--answer :trace-calls (replique-trace-test--calls))
        (switch-to-buffer buffer)
        (replique-trace-untrace-all)
        (replique-trace-test--answer :untrace '(:tag "reply" :untraced ["app/f"]))
        (should (buffer-live-p buffer))
        (should (equal '(:op :trace-calls) (car (car replique-trace-test--asked))))
        ;; traced no more, the calls kept are shown still
        (replique-trace-test--answer
         :trace-calls (plist-put (copy-sequence (replique-trace-test--calls)) :traced nil))
        (let ((text (replique-trace-test--text buffer)))
          (should (string-match-p "^0 functions traced · 4 calls kept" text))
          (should (string-match-p "(app/handle {:n 1}) ⇒ 13" text))
          (should-not (string-match-p "Nothing is traced" text)))
        (replique-trace-untrace-all '(4))
        (should-not (buffer-live-p buffer))
        (replique-trace-test--answer :untrace '(:tag "reply" :untraced []))
        (should (equal '(:op :trace-clear) (car (car replique-trace-test--asked))))))))

(ert-deftest replique-trace-test-the-calls-are-shown-again-as-they-come ()
  "Once for many calls, and only where the buffer is shown."
  (replique-trace-test--with
    (let ((buffer (replique-trace--show process nil)))
      (replique-trace-test--answer :trace-calls (replique-trace-test--calls))
      (switch-to-buffer buffer)
      (replique-trace--called process '(:tag "event" :event "trace-called" :calls 5))
      (replique-trace--called process '(:tag "event" :event "trace-called" :calls 6))
      ;; of ClojureScript, it is another buffer's
      (replique-trace--called process '(:tag "event" :event "trace-called"
                                             :dialect "cljs" :target "node"))
      (should (= 1 (hash-table-count replique-trace--timers)))
      (sleep-for 0.5)
      (should (equal '(:op :trace-calls) (car (car replique-trace-test--asked)))))))

;;; Recorded and made again

(ert-deftest replique-trace-test-a-call-is-made-again ()
  "In the repl of the calls, through the var - which can be private - with
what it was called with."
  (replique-trace-test--with
    (let ((buffer (replique-trace--show process nil))
          (sent nil)
          (dialect nil))
      (replique-trace-test--answer :trace-calls (replique-trace-test--calls))
      (with-current-buffer buffer
        (goto-char (point-min))
        (search-forward "handle")
        (cl-letf (((symbol-function 'replique-trace--repl) (lambda () 'repl))
                  ((symbol-function 'replique-repl-dialect) (lambda (_) dialect))
                  ((symbol-function 'replique-repl-send-code)
                   (lambda (repl code &rest _) (setq sent (list repl code)))))
          (setq dialect :clj)
          (replique-trace-replay)
          (should (equal '(repl "(apply #'app/handle (replique.trace/args-of 1))") sent))
          (setq dialect :cljs)
          (replique-trace-replay)
          (should (equal '(repl "(apply app/handle (replique.trace/args-of 1))") sent)))))))

(ert-deftest replique-trace-test-a-call-is-recorded ()
  "Its function's definition, recorded from its file, in the repl of the calls."
  (replique-trace-test--with
    (let ((buffer (replique-trace--show process nil))
          (file (make-temp-file "replique-trace" nil ".clj"
                                "(ns app)\n\n(defn handle [m]\n  (inc (:n m)))\n"))
          (recorded nil))
      (unwind-protect
          (let ((source (find-file-noselect file)))
            (replique-trace-test--answer
             :trace-calls
             `(:tag "reply" :traced ("app/handle") :kept 1 :dropped 0
               :calls ((:call 1 :var "app/handle" :args ("{:n 1}") :state "returned" :value "2"
                              :made 0 :kept 0 :file ,file :line 3 :column 1))))
            (with-current-buffer buffer
              (goto-char (point-min))
              (search-forward "handle")
              (cl-letf (((symbol-function 'replique-trace--repl) (lambda () 'repl))
                        ((symbol-function 'replique-debug-record)
                         (lambda ()
                           (setq recorded (list (current-buffer)
                                                (buffer-substring (point) (line-end-position))
                                                replique-current-repl)))))
                (replique-trace-record)))
            (should (equal (list source "(defn handle [m]" 'repl) recorded))
            (kill-buffer source))
        (delete-file file)))))

;;; With a process

(ert-deftest replique-trace-test-a-namespace-traced-is-a-tree-of-calls ()
  "The functions of a buffer traced, one called: its calls, a tree."
  (replique-test-with-project dir
    (with-temp-file (expand-file-name "deps.edn" dir)
      (insert "{:paths [\"src\"]}"))
    (make-directory (expand-file-name "src" dir))
    (let ((file (expand-file-name "src/tree.clj" dir)))
      (with-temp-file file
        (insert "(ns tree)\n\n(defn ta [x]\n  (inc x))\n\n"
                "(defn tb [m]\n  (+ (ta (:n m)) (ta 10)))\n"))
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
             (calls nil))
        (unwind-protect
            (save-window-excursion
              (setq repl (replique-repl process))
              (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 60))
              (setq source (find-file-noselect file))
              (with-current-buffer source
                (replique-eval-region (point-min) (point-max)))
              (should (replique-test-wait-for
                       (lambda () (string-match-p "#'tree/tb" (replique-test-text repl))) 30))
              (with-current-buffer source
                (cl-letf (((symbol-function 'replique-process-ensure) (lambda () process)))
                  (replique-trace ?n)))
              (should (replique-test-wait-for
                       (lambda () (setq calls (replique-trace--buffer-of process nil))) 30))
              (replique-repl-send-code repl "(tree/tb {:n 1})")
              (should (replique-test-wait-for
                       (lambda () (string-match-p "^13$" (replique-test-text repl))) 30))
              (with-current-buffer calls (replique-trace-refresh))
              (should (replique-test-wait-for
                       (lambda ()
                         (string-match-p "(tree/tb {:n 1}) ⇒ 13"
                                         (replique-trace-test--text calls)))
                       30))
              (with-current-buffer calls
                (goto-char (point-min))
                (search-forward "tree/tb")
                (replique-trace-toggle))
              (should (replique-test-wait-for
                       (lambda ()
                         (let ((text (replique-trace-test--text calls)))
                           (and (string-match-p "^    (tree/ta 1) ⇒ 2" text)
                                (string-match-p "^    (tree/ta 10) ⇒ 11" text))))
                       30)))
          (when (buffer-live-p calls) (kill-buffer calls))
          (when (buffer-live-p source) (kill-buffer source))
          (when repl
            (replique-conn-close (replique-repl--conn repl))
            (kill-buffer (replique-repl--buffer repl)))
          (replique-kill-process process))))))

(ert-deftest replique-trace-test-a-call-is-recorded-and-made-again ()
  "A traced call's function recorded from the tree, and the call made again:
what its forms evaluated to is said on its code, and the call made again
is in the tree."
  (let ((jar (expand-file-name "../replique-clj/target/clojure-1.12.5-r1.jar"
                               (replique-test-project))))
    (unless (file-exists-p jar) (ert-skip (format "No compiler to record with at %s" jar)))
    (replique-test-with-project dir
      (with-temp-file (expand-file-name "deps.edn" dir)
        (insert (format "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}}}" jar)))
      (make-directory (expand-file-name "src" dir))
      (let ((file (expand-file-name "src/again.clj" dir)))
        (with-temp-file file
          (insert "(ns again)\n\n(defn ta [x]\n  (inc x))\n\n"
                  "(defn- tb [m]\n  (+ (ta (:n m)) 1))\n"))
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
               (calls nil)
               (said (lambda ()
                       (with-current-buffer source
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "(ta (:n")
                           (goto-char (match-beginning 0))
                           (when-let* ((text (replique-debug-eldoc)))
                             (substring-no-properties text)))))))
          (unwind-protect
              (save-window-excursion
                (setq repl (replique-repl process))
                (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 60))
                (setq source (find-file-noselect file))
                (with-current-buffer source
                  (replique-eval-region (point-min) (point-max)))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "#'again/tb" (replique-test-text repl))) 30))
                (with-current-buffer source
                  (cl-letf (((symbol-function 'replique-process-ensure) (lambda () process)))
                    (replique-trace ?n)))
                (should (replique-test-wait-for
                         (lambda () (setq calls (replique-trace--buffer-of process nil))) 30))
                (replique-repl-send-code repl "(#'again/tb {:n 1})")
                (should (replique-test-wait-for
                         (lambda () (string-match-p "^3$" (replique-test-text repl))) 30))
                (with-current-buffer calls (replique-trace-refresh))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "(again/tb {:n 1}) ⇒ 3"
                                                    (replique-trace-test--text calls)))
                         30))
                (with-current-buffer calls
                  (goto-char (point-min))
                  (search-forward "again/tb")
                  (replique-trace-record))
                (should (replique-test-wait-for
                         (lambda () (with-current-buffer source
                                      (replique-debug--marks-in (point-min) (point-max))))
                         30))
                (with-current-buffer calls
                  (goto-char (point-min))
                  (search-forward "again/tb")
                  (replique-trace-replay))
                (should (replique-test-wait-for
                         (lambda () (equal "⇒ 2" (funcall said)))
                         30))
                (should (string-match-p "(apply #'again/tb (replique.trace/args-of [0-9]+))\n3\n"
                                        (replique-test-text repl)))
                (with-current-buffer calls (replique-trace-refresh))
                (should (replique-test-wait-for
                         (lambda ()
                           (with-temp-buffer
                             (insert (replique-trace-test--text calls))
                             (= 2 (count-matches "(again/tb {:n 1}) ⇒ 3" (point-min) (point-max)))))
                         30)))
            (when (buffer-live-p calls) (kill-buffer calls))
            (when (buffer-live-p source) (kill-buffer source))
            (when repl
              (replique-conn-close (replique-repl--conn repl))
              (kill-buffer (replique-repl--buffer repl)))
            (replique-kill-process process)))))))

(provide 'replique-trace-test)

;;; replique-trace-test.el ends here
