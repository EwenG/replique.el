;;; replique-async.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defclass replique-async/chan-impl ()
  ((listeners :initarg :listeners
              :type (or null cons)
              :initform '())
   (providers :initarg :providers
              :type (or null cons)
              :initform '())))

(defun replique-async/chan ()
  (replique-async/chan-impl nil :listeners nil :providers nil))

(let ((ch (replique-async/chan-impl
           nil
           :listeners nil
           :providers nil)))
  (replique-async/<!
   ch (lambda (val)
        (print val)))
  (replique-async/put! ch 3)
  ch)

(defmethod replique-async/<!
  ((ch replique-async/chan-impl) listener-callback)
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
             nil))))

(defmethod replique-async/put!
  ((ch replique-async/chan-impl) item)
  (let ((listener (pop (oref ch listeners))))
    (if listener
        (-let (((&alist :listener-callback listener-callback)
                listener))
          (funcall listener-callback item)
          item)
      (progn (->> `((:item . ,item))
                  list
                  (append (oref ch :providers))
                  (oset ch :providers))
             nil))))

(provide 'replique-async)

;;; replique-async.el ends here
