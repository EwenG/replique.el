;;; replique-record-test.el --- Tests for which functions are recorded  -*- lexical-binding: t; -*-

;;; Commentary:

;; Which functions are recorded, without a process: the requests are kept
;; and answered by the test.  At the bottom, one with a process.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-record)
(require 'replique-recording)

(defvar replique-record-test--asked nil
  "The requests not answered yet, oldest first: (MSG . CALLBACK).")

(defvar replique-record-test--sent nil
  "What was sent to be recorded, the text of each form, oldest first.")

(defmacro replique-record-test--with (&rest body)
  "Run BODY with a process that keeps what it is asked, to be answered.
What is evaluated to be recorded is kept rather than sent."
  (declare (indent 0))
  `(let ((replique-record-test--asked nil)
         (replique-record-test--sent nil)
         (process (replique-process--make :id "record-test" :directory "/tmp/")))
     (ignore process)
     (cl-letf (((symbol-function 'replique-process-live-p) (lambda (p) (and p t)))
               ((symbol-function 'replique-process-ensure) (lambda () process))
               ((symbol-function 'replique-repl-ensure-here) (lambda () 'repl))
               ((symbol-function 'replique-fresh-ensure) #'ignore)
               ((symbol-function 'replique-eval--send)
                (lambda (nodes instrument)
                  (should (eq 'record instrument))
                  (setq replique-record-test--sent
                        (append replique-record-test--sent
                                (mapcar #'replique-parse-text nodes)))))
               ((symbol-function 'replique-process-request)
                (lambda (_process msg &optional callback)
                  (setq replique-record-test--asked
                        (append replique-record-test--asked (list (cons msg callback))))
                  nil)))
       (save-window-excursion ,@body))))

(defun replique-record-test--answer (op frame)
  "Answer the oldest request for OP with FRAME, and return what it asked."
  (let ((cell (or (seq-find (lambda (cell) (eq op (plist-get (car cell) :op)))
                            replique-record-test--asked)
                  (error "Nothing asked %s" op))))
    (setq replique-record-test--asked (delq cell replique-record-test--asked))
    (when (cdr cell) (funcall (cdr cell) frame))
    (car cell)))

(defmacro replique-record-test--with-file (var text &rest body)
  "Run BODY with VAR a .clj file holding TEXT, and its buffer killed after."
  (declare (indent 2))
  `(let ((,var (make-temp-file "replique-record-test" nil ".clj")))
     (unwind-protect
         (progn (with-temp-file ,var (insert ,text))
                ,@body)
       (when-let* ((buffer (find-buffer-visiting ,var)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-file ,var))))

;;; Which functions

(ert-deftest replique-record-test-a-diff-says-which-lines-it-touches ()
  (should (equal '(("src/a.clj" (3 . 4) (10 . 10)) ("src/b.cljs" (1 . 1)))
                 (replique-record--hunks
                  (concat "diff --git a/src/a.clj b/src/a.clj\n"
                          "--- a/src/a.clj\n+++ b/src/a.clj\n"
                          "@@ -3 +3,2 @@ (defn f\n-x\n+y\n+z\n"
                          "@@ -12,2 +10,0 @@\n-a\n-b\n"
                          "diff --git a/src/gone.clj b/src/gone.clj\n"
                          "--- a/src/gone.clj\n+++ /dev/null\n@@ -1 +0,0 @@\n-x\n"
                          "--- a/src/b.cljs\n+++ b/src/b.cljs\n@@ -1 +1 @@\n-x\n+y\n")))))

(ert-deftest replique-record-test-the-functions-a-diff-touches ()
  "The definitions of functions whose lines are touched, by their names in the
namespace of the file - not a macro, nor a def of anything but a fn, which
evaluated again would be made anew, nor what is not a definition."
  (replique-record-test--with-file file
      (concat "(ns app.core)\n\n(defn a [x]\n  x)\n\n(defmacro m [x] x)\n\n"
              "(defn- ^:private b [] 1)\n(def c (atom 0))\n(def d (fn [] 1))\n"
              "(defmethod e :k [_] 1)\n(s/defn f [] 1)\n(comment (a 1))\n")
    (should (equal '("app.core/a") (replique-record--touched file '((4 . 4)))))
    (should (equal '("app.core/b") (replique-record--touched file '((6 . 8)))))
    (should (equal '("app.core/a" "app.core/b" "app.core/d" "app.core/e" "app.core/f")
                   (replique-record--touched file t)))))

(ert-deftest replique-record-test-the-functions-a-branch-changed ()
  "Against a revision: what changed since, committed or not, and new files -
by dialect, and by file."
  (unless (executable-find "git") (ert-skip "No git"))
  (let ((root (file-name-as-directory (make-temp-file "replique-record-git" t))))
    (unwind-protect
        (cl-flet ((git (&rest args) (apply #'replique-record--git root args))
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
          (let ((in (lambda (name) (expand-file-name name root))))
            (should (equal `((:clj (,(funcall in "src/a.clj") "a/g")
                                   (,(funcall in "src/b.cljc") "b/h"))
                             (:cljs (,(funcall in "src/b.cljc") "b/h")
                                    (,(funcall in "src/c.cljs") "c/k")))
                           (replique-record--changed root "HEAD")))))
      (delete-directory root t))))

;;; Recording them

(ert-deftest replique-record-test-a-function-and-its-namespace-are-recorded ()
  "The form at point, as `replique-debug-record' records it - and every
function of the buffer, and nothing else of it.  With
`replique-record-libraries', the library functions they call, by their
calls only."
  (replique-record-test--with
    (cl-letf* ((recorded 0)
               ((symbol-function 'replique-debug-record) (lambda () (cl-incf recorded))))
      (with-temp-buffer
        (replique-clojure-mode)
        (insert "(ns app.core)\n(def state (atom {}))\n(defn f [n]\n  (inc n))\n"
                "(defn g [] (f 1))\n")
        (search-backward "inc")
        (replique-record ?f)
        (should (= 1 recorded))
        (should-not replique-record-test--asked)
        (let ((replique-record-libraries t))
          (replique-record ?f))
        (should (equal '(:op :record-calls :callees-of ["app.core/f"])
                       (replique-record-test--answer
                        :record-calls '(:tag "reply" :recorded ("clojure.string/join") :skipped nil))))
        (let ((replique-record-libraries t))
          (replique-record ?n))
        (should (equal '("(defn f [n]\n  (inc n))" "(defn g [] (f 1))") replique-record-test--sent))
        (should (equal '(:op :record-calls :callees-of ["app.core/f" "app.core/g"])
                       (replique-record-test--answer :record-calls '(:tag "reply"))))))))

(ert-deftest replique-record-test-what-is-around-a-function-is-recorded-from-its-files ()
  "The process says which functions are around it, and where: each file is
opened, and those of its functions evaluated again from there."
  (replique-record-test--with-file other "(ns app.b)\n(defn caller [] (app.a/f 1))\n(defn x [] 2)\n"
    (replique-record-test--with
      (with-temp-buffer
        (replique-clojure-mode)
        (insert "(ns app.a)\n(defn f [n]\n  (inc n))\n")
        (search-backward "inc")
        (replique-record ?a 2)
        (should (equal '(:op :recording-scope :around "app.a/f" :depth 2)
                       (replique-record-test--answer
                        :recording-scope
                        (list :tag "reply" :callers '("app.b/caller") :callees nil
                              :vars (list (list :var "app.b/caller" :file other))))))
        (should (equal '("(defn caller [] (app.a/f 1))") replique-record-test--sent))
        (should (find-buffer-visiting other))))))

(ert-deftest replique-record-test-a-diff-is-recorded-in-the-dialect-of-the-buffer ()
  "A .clj buffer records what changed of Clojure, a .cljs one of
ClojureScript, a .cljc one of the dialect of the repl in hand."
  (replique-record-test--with-file a "(ns a)\n(defn g [] 2)\n"
    (replique-record-test--with
      (cl-letf* ((repl-dialect :clj)
                 (vc-handled-backends nil)
                 ((symbol-function 'locate-dominating-file) (lambda (&rest _) "/tmp/"))
                 ((symbol-function 'replique-record--base) (lambda (_) "master"))
                 ((symbol-function 'read-string) (lambda (_prompt initial) initial))
                 ((symbol-function 'replique-repl-for-dialect) #'ignore)
                 ((symbol-function 'replique-dialect--of-current-repl) (lambda () repl-dialect))
                 ((symbol-function 'replique-record--changed)
                  (lambda (_root _base) `((:clj (,a "a/g")) (:cljs))))
                 (sent (lambda (mode)
                         (with-temp-buffer
                           (funcall mode)
                           (replique-record ?d))
                         (prog1 replique-record-test--sent
                           (setq replique-record-test--sent nil)))))
        (should (equal '("(defn g [] 2)") (funcall sent #'replique-clojure-mode)))
        (should-not (funcall sent #'replique-clojure-clojurescript-mode))
        (should (equal '("(defn g [] 2)") (funcall sent #'replique-clojure-clojurec-mode)))
        (setq repl-dialect :cljs)
        (should-not (funcall sent #'replique-clojure-clojurec-mode))))))

(ert-deftest replique-record-test-what-is-not-recorded-is-said ()
  "How many library functions are recorded by their calls, and why the
others are not, by reason."
  (replique-record-test--with
    (let ((text nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq text (apply #'format fmt args)))))
        (replique-record--libraries process nil '("a/f"))
        (replique-record-test--answer
         :record-calls '(:tag "reply" :recorded ("lib/a")
                              :skipped ((:var "lib/m" :why "a macro")
                                        (:var "lib/p" :why "a protocol's method")
                                        (:var "lib/n" :why "a macro")))))
      (should (equal (concat "replique: the calls of 1 library function recorded, of 3 not - "
                             "a macro (2): lib/m, lib/n; a protocol's method (1): lib/p")
                     text)))))

;;; With a process

(ert-deftest replique-record-test-a-namespace-recorded-calls-a-library ()
  "The functions of a buffer recorded with the library functions they call:
a call of one is in the tree, under the call that made it."
  (let ((jar (expand-file-name "../replique-clj/target/clojure-1.12.5-r1.jar"
                               (replique-test-project))))
    (unless (file-exists-p jar) (ert-skip (format "No compiler to record with at %s" jar)))
    (replique-test-with-project dir
      (with-temp-file (expand-file-name "deps.edn" dir)
        (insert (format "{:paths [\"src\"] :deps {org.clojure/clojure {:local/root %S}}}" jar)))
      (make-directory (expand-file-name "src" dir))
      (let ((file (expand-file-name "src/lib.clj" dir)))
        (with-temp-file file
          (insert "(ns lib (:require [clojure.string :as str]))\n\n"
                  "(defn shout [s]\n  (str/upper-case s))\n\n"
                  "(defn twice [s]\n  (str (shout s) (shout s)))\n"))
        (let* ((replique-coordinates (format "{:local/root %S}" (replique-test-project)))
               (replique-debugger nil)
               (replique-record-libraries t)
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
               (text (lambda () (with-current-buffer calls
                                  (buffer-substring-no-properties (point-min) (point-max))))))
          (unwind-protect
              (save-window-excursion
                (setq repl (replique-repl process))
                (should (replique-test-wait-for (lambda () (replique-repl--ns repl)) 60))
                (setq source (find-file-noselect file))
                ;; loaded, which is what the compiler says what calls what of
                (with-current-buffer source (replique-load-file))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "lib.clj\"}\nnil" (replique-test-text repl)))
                         60))
                (with-current-buffer source
                  (cl-letf (((symbol-function 'replique-process-ensure) (lambda () process)))
                    (replique-record ?n)))
                (should (replique-test-wait-for
                         (lambda () (= 2 (with-current-buffer source
                                           (length (seq-filter
                                                    (lambda (o) (overlay-get o 'replique-debug-unit))
                                                    (overlays-in (point-min) (point-max)))))))
                         30))
                ;; the library functions too, which the process says
                (should (replique-test-wait-for
                         (lambda () (with-current-buffer (messages-buffer)
                                      (string-match-p "the calls of 1 library function recorded"
                                                      (buffer-string))))
                         30))
                (replique-repl-send-code repl "(lib/twice \"a\")")
                (should (replique-test-wait-for
                         (lambda () (string-match-p "\"AA\"" (replique-test-text repl))) 30))
                (with-current-buffer source
                  (cl-letf (((symbol-function 'replique-process-ensure) (lambda () process)))
                    (setq calls (replique-recording-calls))))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "(twice \"a\") ⇒ \"AA\"" (funcall text))) 30))
                (with-current-buffer calls
                  (goto-char (point-min))
                  (search-forward "(twice")
                  (replique-recording-toggle))
                (should (replique-test-wait-for
                         (lambda () (string-match-p "^  ▸ (shout \"a\") ⇒ \"A\"" (funcall text))) 30))
                (with-current-buffer calls
                  (goto-char (point-min))
                  (search-forward "(shout")
                  (replique-recording-toggle))
                ;; under the call that made it
                (should (replique-test-wait-for
                         (lambda () (string-match-p
                                     "^  ▾ (shout \"a\").*\n      (clojure.string/upper-case \"a\") ⇒ \"A\""
                                     (funcall text)))
                         30)))
            (when (buffer-live-p calls) (kill-buffer calls))
            (when (buffer-live-p source)
              (with-current-buffer source (set-buffer-modified-p nil))
              (kill-buffer source))
            (when repl
              (replique-conn-close (replique-repl--conn repl))
              (kill-buffer (replique-repl--buffer repl)))
            (replique-kill-process process)))))))

(provide 'replique-record-test)

;;; replique-record-test.el ends here
