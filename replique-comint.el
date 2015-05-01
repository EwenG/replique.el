;;; replique-comint.el --- -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'comint)

(defun replique/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique/comint-send-input (&optional no-newline artificial)
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond ((and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input no-newline artificial))
              ((comint-after-pmark-p) (comint-accumulate))
              (t (comint-send-input no-newline artificial)))))))

(defun replique/comint-send-input-from-source (input &optional no-newline artificial)
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (comint-add-to-input-history input)
      (run-hook-with-args 'comint-input-filter-functions
                          (if no-newline input
                            (concat input "\n")))

      (comint-snapshot-last-prompt)

      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil)

      (let ((comint-input-sender-no-newline no-newline))
        (funcall comint-input-sender proc input))

      ;; This used to call comint-output-filter-functions,
      ;; but that scrolled the buffer in undesirable ways.
      (run-hook-with-args 'comint-output-filter-functions ""))))

(provide 'replique-comint)

;;; replique-comint.el ends here
