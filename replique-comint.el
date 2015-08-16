;;; replique-comint.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'replique-edn)
(require 'replique-async)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-comint/alist-to-map (alist)
  (let ((m (make-hash-table :test 'equal)))
    (mapcar (-lambda ((k . v))
              (puthash k v m))
            alist)
    m))

(defun replique-comint/tooling-send-msg (buff-props msg chan)
  (-let* (((&alist 'buffer buffer
                   'tooling-chans tooling-chans)
           buff-props)
          (uid (number-to-string (random)))
          (msg (cons `(:uid . ,uid) msg))
          (msg (replique-comint/alist-to-map msg))
          (proc (get-buffer-process buffer)))
    (puthash uid chan tooling-chans)
    (->> (format "(ewen.replique.core/tooling-msg-handle %s)"
                 (replique-edn/pr-str msg))
         (funcall comint-input-sender proc))))

(defvar replique-comint/edn-tag-readers
  `((ewen.replique.core.ToolingMsg
     . ,(-lambda (msg)
          `((type . ,(gethash :type msg))
            (uid . ,(gethash :uid msg))
            (result . ,(gethash :result msg))
            (platform . ,(gethash :platform msg))
            (error . ,(gethash :error msg)))))
    (error . identity)
    (object . identity)))

(defun replique-comint/output-filter (buff-props proc string)
  (-let* (((&alist 'buffer buffer
                   'tooling-chans tooling-chans
                   'edn-reader-state edn-reader-state)
           buff-props)
          (reader (replique-edn/reader
                   nil :str string))
          (msg (cond ((symbol-value edn-reader-state)
                      (-> edn-reader-state
                          symbol-value
                          (replique-edn/set-reader reader)
                          replique-edn/read))
                     ((s-starts-with?
                       "#ewen.replique.core.ToolingMsg"
                       (s-trim-left string))
                      (-> reader
                          replique-edn/init-state
                          (replique-edn/set-tagged-readers
                           replique-comint/edn-tag-readers)
                          replique-edn/read))
                     (t nil)))
          ((&alist :result result
                   :result-state result-state)
           (symbol-value msg)))
    (cond ((equal :waiting (symbol-value result-state))
           (set edn-reader-state msg))
          (msg (let* ((result (car (symbol-value result)))
                      (uid (cdr (assoc 'uid result)))
                      (chan (gethash uid tooling-chans)))
                 (set edn-reader-state nil)
                 (remhash uid tooling-chans)
                 (replique-async/>!
                  chan result
                  (lambda ()
                    (let ((rest-str
                           (replique-edn/reader-rest-string reader)))
                      (when rest-str
                        (->> rest-str
                             (s-chop-prefix "\n")
                             (replique-comint/output-filter
                              buff-props proc))))))))
          (t (comint-output-filter proc string)))))

;; comint-output-filter does not necessarily receive EDN formatted data,
;; thus the splitting on #ewen.replique.core.ToolingMsg. Strings starting
;; with #ewen.replique.core.ToolingMsg are EDN and can be read by
;; replique-edn. Others cannot.
(defun replique-comint/output-filter-dispatch
    (buff-props proc string)
  (let ((s-list (s-slice-at "#ewen.replique.core.ToolingMsg" string)))
    (mapcar (-partial 'replique-comint/output-filter buff-props proc)
            s-list)))

(provide 'replique-comint)

;;; replique-comint.el ends here
