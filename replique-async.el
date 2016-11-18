;; replique-async.el ---   -*- lexical-binding: t; -*-

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

(require 'subr-x)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-async/with-new-dyn-context (fn &rest args)
  (apply 'run-at-time nil nil fn args))

(defclass replique-async/chan-impl ()
  ((listeners :initarg :listeners
              :type (or null cons)
              :initform '())
   (providers :initarg :providers
              :type (or null cons)
              :initform '())
   (closed :initarg :closed
           :type boolean
           :initform nil)))

(defun replique-async/chan ()
  (replique-async/chan-impl nil :listeners nil :providers nil))

(defmethod replique-async/close!
  ((ch replique-async/chan-impl))
  (mapcar (lambda (listener)
            (let-alist listener
              (when .:listener-callback (replique-async/with-new-dyn-context
                                         .:listener-callback nil))))
          (oref ch listeners))
  (mapcar (lambda (provider)
            (let-alist provider
              (when .:provider-callback
                (replique-async/with-new-dyn-context .:provider-callback))))
          (oref ch providers))
  (oset ch listeners '())
  (oset ch providers '())
  (oset ch closed t))

(defmethod replique-async/<!
  ((ch replique-async/chan-impl) listener-callback &optional next-tick)
  (if (oref ch closed)
      (progn (replique-async/with-new-dyn-context listener-callback nil)
             nil)
    (let ((provider (pop (oref ch providers))))
      (if provider
          (let-alist provider
            (replique-async/with-new-dyn-context listener-callback .:item)
            (when .:provider-callback
              (replique-async/with-new-dyn-context .:provider-callback))
            .:item)
        (progn (thread-last `((:listener-callback . ,listener-callback))
                 list
                 (append (oref ch :listeners))
                 (oset ch :listeners))
               nil)))))

(defmethod replique-async/>!
  ((ch replique-async/chan-impl) item provider-callback)
  (cond ((null item) (error "Can't put nil on channel"))
        ((oref ch closed) nil)
        (t (let ((listener (pop (oref ch listeners))))
             (if listener
                 (let-alist listener
                   (replique-async/with-new-dyn-context .:listener-callback item)
                   (replique-async/with-new-dyn-context provider-callback)
                   item)
               (progn (thread-last `((:item . ,item)
                                     (:provider-callback . ,provider-callback))
                        list
                        (append (oref ch :providers))
                        (oset ch :providers))
                      nil))))))

(defmethod replique-async/put!
  ((ch replique-async/chan-impl) item)
  (cond ((null item) (error "Can't put nil on channel"))
        ((oref ch closed) nil)
        (t (let ((listener (pop (oref ch listeners))))
             (if listener
                 (let-alist listener
                   (replique-async/with-new-dyn-context .:listener-callback item)
                   item)
               (progn (thread-last `((:item . ,item))
                        list
                        (append (oref ch :providers))
                        (oset ch :providers))
                      nil))))))

(defun replique-async/default-chan (default-val)
  (let ((out-chan (replique-async/chan)))
    (replique-async/put! out-chan default-val)
    out-chan))


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
 )

(provide 'replique-async)

;;; replique-async.el ends here
