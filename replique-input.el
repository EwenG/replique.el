;; replique-input.el ---   -*- lexical-binding: t; -*-

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

(defun replique-input/send-invisible (proc prompt)
  "Read a string without echoing. Send it to the process standard input.
Security bug: your string can still be temporarily recovered with
\\[view-lossage]; `clear-this-command-keys' can fix that."
  (interactive "P")			; Defeat snooping via C-x ESC ESC
  (let* ((prefix
         (if (eq (window-buffer) (current-buffer))
             ""
           (format "(In buffer %s) " (current-buffer))))
         (str (read-passwd (concat prefix
                                    (or prompt "Non-echoed text: ")))))
    (if (stringp str)
        (progn
          (comint-snapshot-last-prompt)
          (process-send-string proc (concat str "\n")))
      (message "Warning: text will be echoed"))))

;; Java programs read password using the Console class, instead of the Clojure input stream.
;; Thus passwords must be sent to the process standard input instead of the REPL input.
;; Note that normal input (with echoing) can be read using the read-line Clojure function
(defun replique-input/comint-watch-for-password-prompt (string)
  (when-let (tooling-repl (replique/active-repl :tooling nil))
    (when (let ((case-fold-search t))
            (string-match comint-password-prompt-regexp string))
      (when (string-match "^[ \n\r\t\v\f\b\a]+" string)
        (setq string (replace-match "" t t string)))
      (replique-input/send-invisible (clj-data/get tooling-repl :proc) string))))

(defun replique/process-input (input-str)
  (interactive (list (read-string "Process input: ")))
  (let ((tooling-repl (replique/active-repl :tooling t)))
    (process-send-string (clj-data/get tooling-repl :proc) (concat input-str "\n"))))

(defun replique/process-input-password (input-str)
  (interactive (list (read-passwd "Process input: ")))
  (let ((tooling-repl (replique/active-repl :tooling t)))
    (process-send-string (clj-data/get tooling-repl :proc) (concat input-str "\n"))))

(provide 'replique-input)
