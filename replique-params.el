;; replique-params.el ---   -*- lexical-binding: t; -*-

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
(require 'clj-data)
(require 'clj-params)
(require 'replique-repls)
(require 'replique-watch)
(require 'ivy)

(defun replique-params/unqualify (param)
  (string-remove-prefix
   "cljs.core/"
   (string-remove-prefix "clojure.core/" param)))

(defun replique-params/edit-fn (param)
  (cond ((equal "*print-length*" param)
         'clj-params/edit-numerical)
        ((equal "*print-level*" param)
         'clj-params/edit-numerical)
        ((equal "*print-meta*" param)
         'clj-params/edit-boolean)
        ((equal "*warn-on-reflection*" param)
         'clj-params/edit-boolean)))

(defun replique-params/param->param-candidate (k v)
  (let* ((unqualified-k (replique-params/unqualify k)))
    (propertize unqualified-k
                'clj-params/param k
                'clj-params/default-val v
                'clj-params/edit-fn (replique-params/edit-fn unqualified-k))))

(defun replique-params/params->params-candidate (params)
  (let ((candidates nil))
    (maphash (lambda (k v)
               (push (replique-params/param->param-candidate k v) candidates))
             params)
    candidates))

(defun replique-params/params* (params action-fn)
  (ivy-read "Parameters: " (replique-params/params->params-candidate params)
            :require-match t
            :action (apply-partially 'replique-params/edit-param action-fn)))

(defun replique-params/set-param (repl param param-value)
  (let ((buff (clj-data/get repl :buffer)))
    (with-current-buffer buff
      (replique/comint-send-input-from-source
       (concat "(set! " (get-text-property 0 'clj-params/param param) " " param-value ")")))))

(defun replique-params/params-session (repl)
  (let* ((params (clj-data/get repl :params)))
    (clj-params/params* (replique-params/params->params-candidate params)
                        (apply-partially 'replique-params/set-param repl))))

(defun replique-params/params-clj (tooling-repl clj-repl)
  (when clj-repl
    (let* ((params (clj-data/get clj-repl :params)))
      (clj-params/params* (replique-params/params->params-candidate params)
                          (apply-partially 'replique-params/set-param clj-repl)))))

(defun replique-params/params-cljs (tooling-repl cljs-repl)
  (when cljs-repl
    (let* ((params (clj-data/get cljs-repl :params)))
      (clj-params/params* (replique-params/params->params-candidate params)
                          (apply-partially 'replique-params/set-param cljs-repl)))))

(defun replique-params/params-cljc (tooling-repl repl)
  (when repl
    (let* ((params (clj-data/get repl :params)))
      (clj-params/params* (replique-params/params->params-candidate params)
                          (apply-partially 'replique-params/set-param repl)))))

(defun replique/params ()
  (interactive)
  (if (bound-and-true-p replique-watch/minor-mode)
      (replique-watch/params-watch)
    (replique/with-modes-dispatch
     (replique/mode . 'replique-params/params-session)
     (clojure-mode . 'replique-params/params-clj)
     (clojurescript-mode . 'replique-params/params-cljs)
     (clojurec-mode . 'replique-params/params-cljc)
     (t . (user-error "Unsupported major mode: %s" major-mode)))))

(provide 'replique-params)
