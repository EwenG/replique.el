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
              :initform '())
   (synchronous-slot :initarg :synchronous-slot
                     :type t
                     :initform nil)
   (closed :initarg :closed
           :type boolean
           :initform nil)))

(defun replique-async2/chan ()
  (replique-async2/chan-impl nil :listeners nil :providers nil))

(defmethod replique-async2/close!
  ((ch replique-async2/chan-impl))
  (mapcar (-lambda ((&alist :listener-process p
                      :listener-callback l-cb))
            (when p
              (process-send-string p "\n")
              (delete-process p))
            (when l-cb
              (funcall l-cb nil)))
          (oref ch listeners))
  (oset ch closed t))

(defmethod replique-async2/<!
  ((ch replique-async2/chan-impl) listener-callback)
  (if (oref ch closed)
      (progn (funcall listener-callback nil)
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

(defmethod replique-async2/>!
  ((ch replique-async2/chan-impl) item provider-callback)
  (cond ((null item)
         (error "Cannot put nil in channel"))
        ((oref ch closed) nil)
        (let ((listener (pop (oref ch listeners))))
          (if listener
              (-let (((&alist :listener-callback listener-callback
                              :listener-process p)
                      listener))
                (if p
                    (progn
                      (oset ch :synchronous-slot item)
                      (process-send-string p "\n")
                      (delete-process p))
                  (funcall listener-callback item))
                (funcall provider-callback)
                item)
            (progn (->> `((:item . ,item)
                          (:provider-callback . ,provider-callback))
                        list
                        (append (oref ch :providers))
                        (oset ch :providers))
                   nil)))))

(defmethod replique-async2/put!
  ((ch replique-async2/chan-impl) item)
  (cond ((null item)
         (error "Cannot put nil in channel"))
        ((oref ch closed)
         nil)
        (t (let ((listener (pop (oref ch listeners))))
             (if listener
                 (-let (((&alist :listener-callback listener-callback
                                 :listener-process p)
                         listener))
                   (if p
                       (progn
                         (oset ch :synchronous-slot item)
                         (process-send-string p "\n")
                         (delete-process p))
                     (funcall listener-callback item))
                   item)
               (progn (->> `((:item . ,item))
                           list
                           (append (oref ch :providers))
                           (oset ch :providers))
                      nil))))))

(defmethod replique-async2/<!!
  ((ch replique-async2/chan-impl))
  (if (oref ch closed)
      nil
    (let ((provider (pop (oref ch providers))))
      (if provider
          (-let (((&alist :item item
                          :provider-callback provider-callback)
                  provider))
            (when provider-callback
              (funcall provider-callback))
            item)
        (let ((p (start-process "" nil nil)))
          (->> `((:listener-process . ,p))
               list
               (append (oref ch :listeners))
               (oset ch :listeners))
          (accept-process-output p)
          (prog1
              (oref ch :synchronous-slot)
            (oset ch :synchronous-slot nil)))))))


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

 (let ((ch (replique-async2/chan-impl
            nil
            :listeners nil
            :providers nil)))
   (run-at-time "2 sec" nil
                (lambda (ch)
                  (replique-async2/>!
                   ch "r"
                   (lambda ()
                     (print "provided"))))
                ch)
   (let ((val (replique-async2/<!! ch)))
     (print val))
   ch)
 )

(provide 'replique-async2)

;;; replique-async2.el ends here
