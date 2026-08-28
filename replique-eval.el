;;; replique-eval.el --- Evaluating what is in a buffer  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; Sending what is in a buffer to a repl, saying where it came from.
;;
;; The source directive applies to the next form only, so a region holding
;; several forms is sent as several forms, each with its own: evaluating a
;; whole buffer would otherwise place every definition but the first at the
;; line of the first one.
;;
;; Where a form ends is decided by the sexp motion of the major mode, which
;; is what clojure-mode is for.  Replique does not need a mode of its own for
;; this.

;;; Code:

(require 'subr-x)
(require 'replique-edn)
(require 'replique-repl)

(defun replique-src-directive (file line)
  "Return the directive saying that what follows was taken from FILE at LINE.

A repl reads from a socket, so the file and the line numbers the compiler
records mean nothing to an editor unless the client says where the code
came from.  It applies to the next form only, and it is in band rather
than a process wide setting: it cannot race with another repl, and it
stays in order with the code it describes."
  (format "#replique/src %s"
          (replique-edn-map (append (when file (list :file file))
                                    (when line (list :line line))))))

(defun replique-eval--forms (start end)
  "Return the top level forms between START and END.

Each one is a cons of its text and the line it starts on."
  (save-excursion
    (save-restriction
      (widen)
      (let ((forms nil)
            (done nil))
        (goto-char start)
        (while (not done)
          (skip-chars-forward " \t\n\r\f" end)
          (cond
           ((>= (point) end) (setq done t))
           ;; A comment between two forms is not a form, and the reader would
           ;; answer it with a prompt of its own
           ((eq (char-after) ?\;) (forward-line 1))
           (t
            (let ((form-start (point)))
              (condition-case nil
                  (progn
                    (forward-sexp 1)
                    (push (cons (buffer-substring-no-properties form-start (point))
                                (line-number-at-pos form-start t))
                          forms))
                (scan-error
                 ;; Unbalanced: hand what is left over as it is and let the
                 ;; reader say what is wrong with it - it says it better
                 (push (cons (buffer-substring-no-properties form-start end)
                             (line-number-at-pos form-start t))
                       forms)
                 (setq done t)))))))
        (nreverse forms)))))

(defun replique-eval--send (start end)
  "Evaluate what is between START and END in the current repl."
  (let* ((repl (replique-repl-ensure))
         (file (buffer-file-name))
         (forms (replique-eval--forms start end)))
    (unless forms (user-error "Nothing to evaluate"))
    (replique-repl-send-code
     repl
     (mapconcat (lambda (form)
                  (concat (replique-src-directive file (cdr form)) "\n" (car form)))
                forms
                "\n")
     ;; What the buffer is shown leaves the directives out: they are protocol,
     ;; not something anybody wrote
     (mapconcat #'car forms "\n")
     ;; Only one form has one result to show
     (null (cdr forms)))))

;;;###autoload
(defun replique-eval-last-sexp ()
  "Evaluate the form before point."
  (interactive)
  (let ((end (point)))
    (save-excursion
      (backward-sexp)
      (replique-eval--send (point) end))))

;;;###autoload
(defun replique-eval-defun ()
  "Evaluate the top level form around point."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (replique-eval--send (point) end))))

;;;###autoload
(defun replique-eval-region (start end)
  "Evaluate the forms between START and END.

A form that starts inside the region is evaluated whole, even where it
runs past END: half a form is a read error, not an evaluation."
  (interactive "r")
  (replique-eval--send start end))

;;;###autoload
(defun replique-eval-buffer ()
  "Evaluate every form of the current buffer."
  (interactive)
  (replique-eval--send (point-min) (point-max)))

(provide 'replique-eval)

;;; replique-eval.el ends here
