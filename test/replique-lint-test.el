;;; replique-lint-test.el --- Tests for showing what the compiler found  -*- lexical-binding: t; -*-

;;; Commentary:

;; What the process says is wrong with a file is tested where it is worked
;; out.  What is here is where it is shown: a lint is placed only while the
;; text it is about is the text that was compiled, follows its form when the
;; text above it moves, goes when its form is edited - and a verdict about the
;; whole file goes at the first edit.
;;
;; Without a process: what a buffer is given is what the process said, and a
;; plist is a plist however it arrived.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'replique-test)
(require 'replique-lint)

(defconst replique-lint-test--text
  (concat "(ns app.core\n"
          "  (:require [clojure.string :as str]))\n"
          "\n"
          "(defn f [x y]\n"
          "  (inc x))\n"
          "\n"
          "(defn g []\n"
          "  (f 1))\n")
  "A file, as it was compiled.")

(defun replique-lint-test--answer (mtime)
  "What the process said about `replique-lint-test--text\\=' at MTIME."
  (list :analysed t :mtime mtime
        :lints (list (list :type "unused-namespace" :level "warning"
                           :message "namespace clojure.string is required but never used"
                           :line 2 :column 14 :end-line 2 :end-column 28 :scope "file")
                     (list :type "unused-binding" :level "warning"
                           :message "unused binding y"
                           :line 4 :column 12 :end-line 4 :end-column 13 :scope "form")
                     (list :type "invalid-arity" :level "error"
                           :message "app.core/f is called with 1 arg but expects 2"
                           :line 8 :column 3 :end-line 8 :end-column 8 :scope "form"))))

(defmacro replique-lint-test--visiting (&rest body)
  "Run BODY in a buffer visiting a file holding the compiled text."
  (declare (indent 0))
  `(let* ((file (make-temp-file "replique-lint" nil ".clj" replique-lint-test--text))
          (buffer (let ((replique-lint-flymake nil)
                        (replique-clojure-mode-hook nil))
                    (find-file-noselect file))))
     (unwind-protect
         (with-current-buffer buffer
           ;; the edits are noted through the hook this mode adds
           (add-hook 'before-change-functions #'replique-lint--note-change nil t)
           ,@body)
       (with-current-buffer buffer (set-buffer-modified-p nil))
       (kill-buffer buffer)
       (delete-file file))))

(defun replique-lint-test--shown ()
  "What is shown, as (TEXT MESSAGE) - the text each lint is placed over."
  (mapcar (lambda (d)
            (list (buffer-substring-no-properties (plist-get d :beginning)
                                                  (plist-get d :end))
                  (plist-get d :message)))
          (replique-lint-diagnostics)))

(ert-deftest replique-lint-test-shown-where-the-compiled-text-is ()
  "Positions taken as they come while the buffer is the version they are
about."
  (replique-lint-test--visiting
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer (replique-lint--visited-mtime)))))
    (should (equal '(("clojure.string" "namespace clojure.string is required but never used")
                     ("y" "unused binding y")
                     ("(f 1)" "app.core/f is called with 1 arg but expects 2"))
                   (replique-lint-test--shown)))))

(ert-deftest replique-lint-test-another-version-shows-nothing ()
  "An answer about a version the buffer is not - saved since, or edited before
the answer came - has no positions anybody can use."
  (replique-lint-test--visiting
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer
                            (1+ (replique-lint--visited-mtime))))))
    (should (null (replique-lint-diagnostics))))
  (replique-lint-test--visiting
    (goto-char (point-max))
    (insert "\n")
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer (replique-lint--visited-mtime)))))
    (should (null (replique-lint-diagnostics)))))

(ert-deftest replique-lint-test-a-lint-follows-its-form ()
  "Text typed above a form moves it, and its lints with it."
  (replique-lint-test--visiting
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer (replique-lint--visited-mtime)))))
    (replique-lint-diagnostics)
    (goto-char (point-min))
    (search-forward "(defn f")
    (beginning-of-line)
    (insert "(def a 1)\n\n")
    (should (equal '(("y" "unused binding y")
                     ("(f 1)" "app.core/f is called with 1 arg but expects 2"))
                   (replique-lint-test--shown)))))

(ert-deftest replique-lint-test-an-edited-form-loses-its-lints ()
  "What a lint says about a form is about the form as it was compiled - and
only that form loses them."
  (replique-lint-test--visiting
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer (replique-lint--visited-mtime)))))
    (replique-lint-diagnostics)
    (goto-char (point-min))
    (search-forward "(f 1")
    (insert " 2")
    ;; and the require nothing used, a verdict about the whole file, went at
    ;; the first edit wherever it was
    (should (equal '(("y" "unused binding y")) (replique-lint-test--shown)))))

(ert-deftest replique-lint-test-the-same-version-loaded-again-brings-them-back ()
  "An edit undone, and the file loaded again, is the compiled text again."
  (replique-lint-test--visiting
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer (replique-lint--visited-mtime)))))
    (replique-lint-diagnostics)
    (goto-char (point-min))
    (search-forward "(f 1")
    (insert " 2")
    (delete-char -2)
    (set-buffer-modified-p nil)
    (should (= 3 (length (replique-lint-diagnostics))))))

(ert-deftest replique-lint-test-both-compilers-of-a-cljc-file ()
  "What both say is said once, and what one says names which."
  (replique-lint-test--visiting
    (let ((mtime (replique-lint--visited-mtime)))
      (setq replique-lint--answers
            (list (cons :clj (replique-lint-test--answer mtime))
                  (cons :cljs (list :mtime mtime
                                    :lints (list (list :type "unused-binding" :level "warning"
                                                       :message "unused binding y"
                                                       :line 4 :column 12 :end-line 4
                                                       :end-column 13 :scope "form")))))))
    (should (equal '("[clj] namespace clojure.string is required but never used"
                     "unused binding y"
                     "[clj] app.core/f is called with 1 arg but expects 2")
                   (mapcar #'cadr (replique-lint-test--shown))))))

(ert-deftest replique-lint-test-flymake-is-told-each-level ()
  "An error, a warning, and the type after the message."
  (replique-lint-test--visiting
    (setq replique-lint--answers
          (list (cons :clj (replique-lint-test--answer (replique-lint--visited-mtime)))))
    (let (reported)
      (replique-lint-flymake (lambda (diagnostics) (setq reported diagnostics)))
      (should (equal '(:warning :warning :error)
                     (mapcar #'flymake-diagnostic-type reported)))
      (should (equal "unused binding y [unused-binding]"
                     (flymake-diagnostic-text (cadr reported)))))))

(ert-deftest replique-lint-test-asked-again-where-it-is-shown ()
  "A buffer in a window asks at once; one nobody is looking at is behind, and
asks when it is shown."
  (let ((asked nil)
        (shown (generate-new-buffer "shown"))
        (hidden (generate-new-buffer "hidden")))
    (unwind-protect
        (cl-letf (((symbol-function 'replique-lint-refresh)
                   (lambda (&optional buffer)
                     (push buffer asked)
                     (with-current-buffer buffer
                       (setq replique-lint--generation replique-lint--latest)))))
          (dolist (b (list shown hidden))
            (with-current-buffer b (setq-local replique-lint-mode t)))
          (save-window-excursion
            (switch-to-buffer shown)
            (replique-lint--on-analysis nil '(:generation 1))
            (should (equal (list shown) asked))
            (should (replique-lint--behind-p hidden))
            (switch-to-buffer hidden)
            (replique-lint--on-shown (selected-frame))
            (should (equal (list hidden shown) asked))))
      (kill-buffer shown)
      (kill-buffer hidden))))

(provide 'replique-lint-test)

;;; replique-lint-test.el ends here
