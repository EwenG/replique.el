;;; ob-replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((clojure-mode "4.0.1") (replique "0.0.1") (dash "2.11.0"))
;;; Commentary:

;;; Code:

(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))
(add-to-list 'org-babel-tangle-lang-exts '("clojurescript" . "cljs"))
;;(add-to-list 'org-src-lang-modes '("clojure" . replique/mode))
;;(add-to-list 'org-src-lang-modes '("clojurescript" . replique/mode))

(defvar org-babel-default-header-args:clojure '())
(defvar org-babel-default-header-args:clojurescript '())

(defun ob-replique-expand-body
    (body params &optional processed-params)
  (let ((vars (mapcar 'cdr (org-babel-get-header
                            processed-params
                            :var))))
    (concat
     "(let ["
     (mapconcat
      (lambda (pair)
        (format
         "%s %s"
         (car pair)
         (cdr pair)))
      vars "\n")
     "]\n" body ")\n")))

(defun org-babel-expand-body:clojure
    (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (ob-replique-expand-body body params processed-params))

(defun org-babel-expand-body:clojurescript
    (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (ob-replique-expand-body body params processed-params))

(defun org-babel-execute:clojure (body params)
  "Execute a block of clojure code with org-babel.
  This function is called by `org-babel-execute-src-block'"
  (message "executing clojure source code block")
  (require 'replique)
  (let* ((processed-params (org-babel-process-params params))
         (result-params (cdr (org-babel-get-header
                              processed-params
                              :result-params)))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (org-babel-get-header
                            processed-params
                            :result-type)))
         (full-body (org-babel-expand-body:clojure
                     body params processed-params)))
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;;
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    (condition-case nil
        (replique/eval-form :clj full-body)
      (error "Error occured during evaluation"))))

(defun org-babel-execute:clojurescript (body params)
  "Execute a block of clojurescript code with org-babel.
  This function is called by `org-babel-execute-src-block'"
  (message "executing clojurescript source code block")
  (require 'replique)
  (let* ((processed-params (org-babel-process-params params))
         (result-params (cdr (org-babel-get-header
                              processed-params
                              :result-params)))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (org-babel-get-header
                            processed-params
                            :result-type)))
         (full-body (org-babel-expand-body:clojurescript
                     body params processed-params)))
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;;
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    (condition-case nil
        (replique/eval-form :cljs full-body)
      (error "Error occured during evaluation"))))

(provide 'ob-replique)

;;; ob-replique.el ends here
