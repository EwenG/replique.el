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

(require 'replique-hashmap)

(defvar replique-params/print-length-history nil)
(defvar replique-params/print-level-history nil)
(defvar replique-params/warn-on-reflection-history nil)

(defun replique-params/param->history (param)
  (cond ((string-suffix-p "*print-length*" param)
         'replique-params/print-length-history)
        ((string-suffix-p "*print-level*" param)
         'replique-params/print-level-history)
        ((string-suffix-p "*warn-on-reflection*" param)
         'replique-params/warn-on-reflection-history)))

(defun replique-params/unqualify (param)
  (string-remove-prefix
   "cljs.core/"
   (string-remove-prefix "clojure.core/" param)))

(defun replique-params/boolean->string (b)
  (if b "true" "false"))

(defun replique-params/minibuffer-numeric-inc ()
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (cond ((equal "" content)
           (delete-minibuffer-contents)
           (insert "nil"))
          ((equal "nil" content)
           (delete-minibuffer-contents)
           (insert "0"))
          (t
           (delete-minibuffer-contents)
           (insert (number-to-string (+ 1 (string-to-number content))))))))

(defun replique-params/minibuffer-numeric-dec ()
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (cond ((equal "0" content)
           (delete-minibuffer-contents)
           (insert "nil"))
          (t
           (let ((n (string-to-number content)))
             (when (> n 0)
               (delete-minibuffer-contents)
               (insert (number-to-string (- n 1)))))))))

(defun replique-params/minibuffer-boolean-toggle ()
  (interactive)
  (let ((content (minibuffer-contents-no-properties)))
    (cond ((equal "true" content)
           (delete-minibuffer-contents)
           (insert "false"))
          ((equal "false" content)
           (delete-minibuffer-contents)
           (insert "true"))
          (t
           (delete-minibuffer-contents)
           (insert "false")))))

(defvar replique-params/minibuffer-map-numerical
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "+" 'replique-params/minibuffer-numeric-inc)
    (define-key map "-" 'replique-params/minibuffer-numeric-dec)
    map))

(defvar replique-params/minibuffer-map-boolean
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "+" 'replique-params/minibuffer-boolean-toggle)
    (define-key map "-" 'replique-params/minibuffer-boolean-toggle)
    map))

(defun replique-params/edit-numerical (param default-val action-fn)
  (let ((continue t)
        (value nil)
        (history-add-new-input nil))
    (while continue
      (setq value (read-from-minibuffer (concat (replique-params/unqualify param) ": ")
                                        default-val
                                        replique-params/minibuffer-map-numerical
                                        nil
                                        (replique-params/param->history param)
                                        default-val))
      (cond ((equal "nil" value)
             (add-to-history (replique-params/param->history param) value)
             (setq value value)
             (setq continue nil))
            ((string-match-p "^[0-9]+$" value)
             (add-to-history (replique-params/param->history param) value)
             (setq value value)
             (setq continue nil)))
      (when continue
        (message (concat param " must be nil or a positive numeric value"))
        (sit-for 1)))
    (funcall action-fn param value)))

(defun replique-params/edit-boolean (param default-val action-fn)
  (let ((continue t)
        (value nil)
        (history-add-new-input nil))
    (while continue
      (setq value (read-from-minibuffer (concat (replique-params/unqualify param) ": ")
                                        default-val
                                        replique-params/minibuffer-map-boolean
                                        nil
                                        (replique-params/param->history param)
                                        default-val))
      (when (or (equal "false" value) (equal "true" value))
        (add-to-history (replique-params/param->history param) value)
        (setq value value)
        (setq continue nil))
      (when continue
        (message (concat param " must be a boolean (true or false)"))
        (sit-for 1)))
    (funcall action-fn param value)))

(defun replique-params/edit-param (params action-fn param)
  (let ((param (cadr param)))
    (cond ((string-suffix-p "*print-length*" param)
           (replique-params/edit-numerical
            param (prin1-to-string (replique/get params param)) action-fn))
          ((string-suffix-p "*print-level*" param)
           (replique-params/edit-numerical
            param (prin1-to-string (replique/get params param)) action-fn))
          ((string-suffix-p "*warn-on-reflection*" param)
           (replique-params/edit-boolean
            param (replique-params/boolean->string (replique/get params param)) action-fn)))))

(defun replique-params/param->param-candidate (param)
  `(,(replique-params/unqualify param) ,param))

(defun replique-params/params* (params action-fn)
  (ivy-read "Parameters: " (mapcar 'replique-params/param->param-candidate
                                   (replique/keys params))
            :require-match t
            :action (apply-partially 'replique-params/edit-param params action-fn)))

(defun replique-params/set-param (repl param param-value)
  (let ((buff (replique/get repl :buffer)))
    (with-current-buffer buff
      (replique/comint-send-input-from-source
       (concat "(set! " param " " param-value ")")))))

(defun replique-params/params-session (repl)
  (replique-params/params* (replique/get repl :params)
                           (apply-partially 'replique-params/set-param repl)))

(defun replique-params/params-clj (tooling-repl clj-repl)
  (when clj-repl
    (replique-params/params* (replique/get clj-repl :params)
                             (apply-partially 'replique-params/set-param clj-repl))))

(defun replique-params/params-cljs (tooling-repl cljs-repl)
  (when cljs-repl
    (replique-params/params* (replique/get cljs-repl :params)
                             (apply-partially 'replique-params/set-param cljs-repl))))

(defun replique-params/params-cljc (tooling-repl repl)
  (when repl
    (replique-params/params* (replique/get repl :params)
                             (apply-partially 'replique-params/set-param repl))))

(defun replique/params ()
  (interactive)
  (replique/with-modes-dispatch
   (replique/mode . 'replique-params/params-session)
   (clojure-mode . 'replique-params/params-clj)
   (clojurescript-mode . 'replique-params/params-cljs)
   (clojurec-mode . 'replique-params/params-cljc)
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(provide 'replique-params)
