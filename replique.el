;;; replique.el --- A development environment for Clojure  -*- lexical-binding: t; -*-

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

;; Version 2.0.0-SNAPSHOT
;; Package-Requires: ((emacs "30"))

;;; Commentary:

;; The entry point: what to turn on in a Clojure buffer.
;;
;;   M-x replique-start      start a process in a directory and connect to it
;;   M-x replique-connect    connect to one that is already running
;;   M-x replique-repl       open a repl on it
;;
;; This is the editor client of the replique protocol, and no more than that.
;; Completion, documentation and finding a definition are the tooling ops,
;; which the process does not answer yet.

;;; Code:

(require 'replique-common)
(require 'replique-edn)
(require 'replique-conn)
(require 'replique-process)
(require 'replique-repl)
(require 'replique-eval)

(defvar replique-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-e") #'replique-eval-last-sexp)
    (define-key map (kbd "C-M-x") #'replique-eval-defun)
    (define-key map (kbd "C-c C-r") #'replique-eval-region)
    (define-key map (kbd "C-c C-k") #'replique-eval-buffer)
    (define-key map (kbd "C-c C-c") #'replique-interrupt)
    (define-key map (kbd "C-c C-z") #'replique-switch-to-repl)
    (define-key map (kbd "C-c C-o") #'replique-show-process-output)
    (define-key map (kbd "C-c C-e") #'replique-show-last-exception)
    map)
  "Keymap of `replique-mode'.")

;;;###autoload
(define-minor-mode replique-mode
  "Evaluate what is in this buffer in a replique repl.

\\{replique-mode-map}"
  :lighter " replique"
  :keymap replique-mode-map)

;;;###autoload
(defun replique-version ()
  "Say which replique this is."
  (interactive)
  (message "replique 2.0.0-SNAPSHOT, protocol version 1"))

(provide 'replique)

;;; replique.el ends here
