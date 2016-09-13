;;; repliquedoc.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24"))
;;; Commentary:

;;; Code:

(defun replique/eldoc-documentation-function (callback)
  (let* ((tooling-repl (replique/active-repl :tooling))
         (tooling-chan (-h/get tooling-repl :chan)))
    (when tooling-chan
      (replique/send-tooling-msg
       tooling-repl
       (-h/hash-map :type :repliquedoc
                    :context (replique/form-with-point)
                    :ns (clojure-find-ns)
                    :symbol (symbol-at-point)))
      (replique-async/<!
       tooling-chan
       (lambda (resp)
         (when resp
           (let ((err (-h/get resp :error)))
             (if err
                 (progn
                   (message (replique-edn/pr-str err))
                   (message "eldoc failed"))
               (funcall callback (-h/get resp :doc))))))))))

(defun replique/eldoc-schedule-timer ()
  (or (and eldoc-timer
           (memq eldoc-timer timer-idle-list))
      (setq eldoc-timer
            (run-with-idle-timer
	     eldoc-idle-delay t
	     (lambda () (and eldoc-mode (replique/eldoc-print-current-symbol-info))))))

  ;; If user has changed the idle delay, update the timer.
  (cond ((not (= eldoc-idle-delay eldoc-current-idle-delay))
         (setq eldoc-current-idle-delay eldoc-idle-delay)
         (timer-set-idle-time eldoc-timer eldoc-idle-delay t))))

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
	(eldoc-mode)
        (remove-hook 'post-command-hook 'eldoc-schedule-timer t)
        (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area t)
        (when eldoc-timer
          (cancel-timer eldoc-timer)
          (setq eldoc-timer nil))
        (add-hook 'post-command-hook 'replique/eldoc-schedule-timer nil t))
    (eldoc-mode -1)
    (remove-hook 'post-command-hook 'replique/eldoc-schedule-timer t)))

(provide 'repliquedoc)

;; repliquedoc.el ends here
