;; replique-utils.el ---   -*- lexical-binding: t; -*-

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

(defun replique/keyword-to-string (k)
  (substring (symbol-name k) 1))

(defun replique/message-nolog (format-string)
  (let ((message-log-max nil))
    (message "%s" format-string)))

(defun replique/visible-buffers ()
  (let ((buffers '()))
    (walk-windows
     (lambda (window) (push (window-buffer window) buffers))
     nil 'visible)
    buffers))

(defmacro replique/return-value-on-quit (value &rest body)
  (let ((ret-sym (make-symbol "ret-sym")))
    `(let ((inhibit-quit t))
       (let ((,ret-sym (with-local-quit ,@body)))
         (if (null quit-flag)
             ,ret-sym
           (setq quit-flag nil)
           ,value)))))

;; completing-read uses an alphabetical order by default. This function can be used too keep
;; the order of the candidates
(defun replique/presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,'identity)
                   (cycle-sort-function . ,'identity))
      (complete-with-action action completions string pred))))

(provide 'replique-utils)
