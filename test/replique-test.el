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
(require 'replique)

(defvar replique-test-process nil
  "The process shared by the tests.")

(defun replique-test-project ()
  "Return the Clojure project to start a process in, or skip the test."
  (let ((project (getenv "REPLIQUE_PROJECT")))
    (unless project
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
`replique-start' passes."
  (let* ((project (replique-test-project))
         (default-directory project)
         (known replique-processes))
    (if options
        (make-process :name "replique-test" :buffer nil
                      :command (list replique-clojure-program "-M" "-m" "replique.main" options)
                      :coding 'utf-8-unix :noquery t)
      (replique-start project))
    (unless options
      (unless (replique-test-wait-for
               (lambda () (seq-difference replique-processes known)) 120)
        (error "The process did not start"))
      (car (seq-difference replique-processes known)))))

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
    (let ((exception (replique-repl--last-exception repl)))
      (should (equal "clojure.lang.ExceptionInfo" (plist-get exception :class)))
      (should (plist-get exception :trace)))))

(ert-deftest replique-test-a-cut-trace-is-shown-as-cut ()
  "A frame carries the top of the trace and the outermost causes.  What it
left out has to be said: the root cause is the one the reported message
names, and 64 of 300 frames shown as a whole trace is a lie."
  (replique-test-with-repl repl
    (should (string-match-p "more frames"
                            (replique-test-eval repl "((fn f [n] (inc (f (inc n)))) 0)")))
    (should (plist-get (replique-repl--last-exception repl) :trace-dropped))))

(ert-deftest replique-test-nothing-is-said-about-what-was-not-cut ()
  (replique-test-with-repl repl
    (replique-test-eval repl "(throw (Exception. \"shallow\"))")
    (let ((exception (replique-repl--last-exception repl)))
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

;;; Cleaning up

(defun replique-test-tear-down ()
  "Stop the process the tests shared."
  (when (replique-process-live-p replique-test-process)
    (replique-kill-process replique-test-process))
  (setq replique-test-process nil))

(add-hook 'kill-emacs-hook #'replique-test-tear-down)

(provide 'replique-test)

;;; replique-test.el ends here
