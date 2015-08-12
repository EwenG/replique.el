;;; replique-async.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defclass replique-async/chan-impl ()
  ((listeners :initarg :listeners
              :type (or null cons)
              :initform '())
   (providers :initarg :providers
              :type (or null cons)
              :initform '())
   (closed :initarg :closed
           :type (or t nil)
           :initform nil)))

(defun replique-async/chan ()
  (replique-async/chan-impl nil :listeners nil :providers nil))

(defmethod replique-async/close!
  ((ch replique-async/chan-impl))
  (mapcar (-lambda ((&alist :listener-process p))
            (when p
              (process-send-string p "\n")
              (delete-process p)))
   (oref ch listeners))
  (oset ch closed t))

(defmethod replique-async/<!
  ((ch replique-async/chan-impl) listener-callback)
  (if (oref ch closed)
      (progn
        (listener-callback)
        nil)
      (let ((provider (pop (oref ch providers))))
        (if provider
            (-let (((&alist :item item
                            :provider-callback provider-callback)
                    provider))
              (funcall listener-callback item)
              (when provider-callback
                (funcall provider-callback))
              item)
          (progn (->> `((:listener-callback . ,listener-callback))
                      list
                      (append (oref ch :listeners))
                      (oset ch :listeners))
                 nil)))))

(defmethod replique-async/>!
  ((ch replique-async/chan-impl) item provider-callback)
  (if (oref ch closed)
      (progn
        (provider-callback)
        nil)
    (let ((listener (pop (oref ch listeners))))
      (if listener
          (-let (((&alist :listener-callback listener-callback
                          :listener-process p)
                  listener))
            (when p
              (process-send-string p "\n")
              (delete-process p))
            (funcall listener-callback item)
            item)
        (progn (->> `((:item . ,item)
                      (:provider-callback . ,provider-callback))
                    list
                    (append (oref ch :providers))
                    (oset ch :providers))
               nil)))))

(defmethod replique-async/put!
  ((ch replique-async/chan-impl) item)
  (if (oref ch closed)
      nil
    (let ((listener (pop (oref ch listeners))))
      (if listener
          (-let (((&alist :listener-callback listener-callback
                          :listener-process p)
                  listener))
            (when p
              (process-send-string p "\n")
              (delete-process p))
            (funcall listener-callback item)
            item)
        (progn (->> `((:item . ,item))
                    list
                    (append (oref ch :providers))
                    (oset ch :providers))
               nil)))))

(defmethod replique-async/<!!
  ((ch replique-async/chan-impl) listener-callback)
  (if (oref ch closed)
      (progn
        (listener-callback)
        nil)
    (let ((provider (pop (oref ch providers))))
      (if provider
          (-let (((&alist :item item
                          :provider-callback provider-callback)
                  provider))
            (funcall listener-callback item)
            (when provider-callback
              (funcall provider-callback))
            item)
        (let ((p (start-process "" nil nil)))
          (->> `((:listener-callback . ,listener-callback)
                 (:listener-process . ,p))
               list
               (append (oref ch :listeners))
               (oset ch :listeners))
          (accept-process-output p)
          nil)))))


(comment
 (let ((ch (replique-async/chan-impl
            nil
            :listeners nil
            :providers nil)))
   (replique-async/<!
    ch (lambda (val)
         (print val)))
   (replique-async/put! ch 3)
   ch)

 (let ((ch (replique-async/chan-impl
            nil
            :listeners nil
            :providers nil)))
   (replique-async/>!
    ch "r"
    (lambda ()
      (print "provided")))
   (replique-async/<!
    ch
    (lambda (val)
      (print val)))
   ch)

 (let ((ch (replique-async/chan-impl
            nil
            :listeners nil
            :providers nil)))
   (run-at-time "2 sec" nil
                (lambda (ch)
                  (replique-async/>!
                   ch "r"
                   (lambda ()
                     (print "provided"))))
                ch)
   (replique-async/<!!
    ch
    (lambda (val)
      (print val)))
   ch)
 )

(provide 'replique-async)

;;; replique-async.el ends here
