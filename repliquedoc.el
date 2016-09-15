;;; repliquedoc.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24"))
;;; Commentary:

;;; Code:

(defvar replique/eldoc-timer nil)

(defun replique/eldoc-schedule-timer ()
  (or (and replique/eldoc-timer
           (memq replique/eldoc-timer timer-idle-list))
      (setq replique/eldoc-timer
            (run-with-idle-timer
	     eldoc-idle-delay t
	     (lambda () (and repliquedoc-mode (replique/eldoc-print-current-symbol-info))))))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= eldoc-idle-delay eldoc-current-idle-delay))
         (setq eldoc-current-idle-delay eldoc-idle-delay)
         (timer-set-idle-time replique/eldoc-timer eldoc-idle-delay t))))

(defun replique/eldoc-print-current-symbol-info ()
  (with-demoted-errors "eldoc error: %s"
    (and (or (eldoc-display-message-p)
             ;; Erase the last message if we won't display a new one.
             (when eldoc-last-message
               (eldoc-message nil)
               nil))
	 (when eldoc-documentation-function
           (funcall eldoc-documentation-function
                    (lambda (doc)
                      (with-demoted-errors "eldoc error: %s"
                        (eldoc-message doc))))))))

;;;###autoload
(define-minor-mode repliquedoc-mode
  "Piggieback on eldoc, but make eldoc-documentation-function asynchronous"
  :group 'repliquedoc
  (if repliquedoc-mode
      (progn
	(setq eldoc-last-message nil)
        (add-hook 'post-command-hook 'replique/eldoc-schedule-timer nil t))
    (kill-local-variable 'eldoc-message-commands)
    (remove-hook 'post-command-hook 'replique/eldoc-schedule-timer t)))

(provide 'repliquedoc)

;; repliquedoc.el ends here
