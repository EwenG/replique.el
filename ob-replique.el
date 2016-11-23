;; ob-replique.el ---   -*- lexical-binding: t; -*-

;; Copyright © 2016 Ewen Grosjean

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;; Commentary:

;; Code:

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
  (require 'replique2)
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
    (condition-case err
        (replique/eval-form :clj full-body)
      (error
       (message (error-message-string err))
       "Error occured during evaluation"))))

(defun org-babel-execute:clojurescript (body params)
  "Execute a block of clojurescript code with org-babel.
  This function is called by `org-babel-execute-src-block'"
  (message "executing clojurescript source code block")
  (require 'replique2)
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
    (condition-case err
        (replique/eval-form :cljs full-body)
      (error
       (message (error-message-string err))
       "Error occured during evaluation"))))

(provide 'ob-replique)

;;; ob-replique.el ends here
