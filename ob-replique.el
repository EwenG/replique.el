;;; ob-replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((clojure-mode "4.0.1") (replique "0.0.1") (dash "2.11.0"))
;;; Commentary:

;;; Code:

(require 'ob)

(add-to-list 'org-babel-tangle-lang-exts '("replique" . "clj"))
(add-to-list 'org-babel-tangle-lang-exts '("replique" . "cljs"))
(add-to-list 'org-babel-tangle-lang-exts '("replique" . "cljc"))
(add-to-list 'org-src-lang-modes '("replique" . clojure))

(defvar org-babel-default-header-args:replique '())

(defun org-babel-expand-body:replique
    (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
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

(defun org-babel-execute:replique (body params)
  "Execute a block of clojure/clojurescript code with org-babel.
  This function is called by `org-babel-execute-src-block'"
  (message "executing clojure/clojurescript source code block")
  (require 'dash)
  (require 'replique-edn)
  (require 'replique)
  (let* ((processed-params (org-babel-process-params params))
         (result-params (cdr (org-babel-get-header
                              processed-params
                              :result-params)))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (cdr (org-babel-get-header
                            processed-params
                            :result-type)))
         (full-body (org-babel-expand-body:replique
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
        (-> (replique/eval-form full-body)
            replique-edn/pr-str)
      (error "Error occured during evaluation"))))

(provide 'ob-replique)

;;; ob-replique.el ends here
