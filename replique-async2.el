;;; replique-async2.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defclass replique-async2/chan-impl ()
  ((listeners :initarg :listeners
              :type (or null cons)
              :initform '())
   (providers :initarg :providers
              :type (or null cons)
              :initform '())))

(defun replique-async2/chan ()
  (replique-async2/chan-impl nil :listeners nil :providers nil))

(defmethod replique-async2/<!
  ((ch replique-async2/chan-impl) listener-callback)
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

(defmethod replique-async2/>!
  ((ch replique-async2/chan-impl) item provider-callback)
  (let ((listener (pop (oref ch listeners))))
    (if listener
        (-let (((&alist :listener-callback listener-callback) listener))
          (funcall listener-callback item)
          (funcall provider-callback)
          item)
      (progn (->> `((:item . ,item)
                    (:provider-callback . ,provider-callback))
                  list
                  (append (oref ch :providers))
                  (oset ch :providers))
             nil))))

(defmethod replique-async2/put!
  ((ch replique-async2/chan-impl) item)
  (let ((listener (pop (oref ch listeners))))
    (if listener
        (-let (((&alist :listener-callback listener-callback) listener))
          (funcall listener-callback item)
          item)
      (progn (->> `((:item . ,item))
                  list
                  (append (oref ch :providers))
                  (oset ch :providers))
             nil))))


(comment
 (let ((ch (replique-async2/chan-impl
            nil
            :listeners nil
            :providers nil)))
   (replique-async2/<!
    ch (lambda (val)
         (print val)))
   (replique-async2/put! ch 3)
   ch)

 (let ((ch (replique-async2/chan-impl
            nil
            :listeners nil
            :providers nil)))
   (replique-async2/>!
    ch "r"
    (lambda ()
      (print "provided")))
   (replique-async2/<!
    ch
    (lambda (val)
      (print val)))
   ch)
 )

(provide 'replique-async2)

;;; replique-async2.el ends here
