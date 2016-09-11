;;; dashhash.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24"))
;;; Commentary:

;;; Code:

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun -h/hash-map (&rest data)
  (let ((l (length data)))
    (when (not (= 0 (logand l 1)))
      (user-error "Map must contain an even number of forms"))
    (let ((m (make-hash-table :test 'equal))
          (data-rest data))
      (while data-rest
        (puthash (car data-rest) (cadr data-rest) m)
        (setq data-rest (cddr data-rest)))
      m)))

(defun -h/get (hash key &optional default)
  (if (null hash)
      nil
    (gethash key hash default)))

(defun -h/assoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (puthash (car kvs) (cadr kvs) hash)
      (-h/assoc-helper hash (cddr kvs)))))

(defun -h/assoc (hash &rest kvs)
  (let ((args-length (1+ (length kvs))))
    (when (= 0 (logand 1 args-length))
      (user-error
       "-h/assoc expects even number of arguments after hashtable, found odd number"))
    (-h/assoc-helper (copy-hash-table (or hash (-h/hash-map))) kvs)))

(defun -h/dissoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (remhash (car kvs) hash)
      (-h/dissoc-helper hash (cdr kvs)))))

(defun -h/dissoc (hash &rest kvs)
  (-h/dissoc-helper (copy-hash-table (or hash (-h/hash-map))) kvs))

(defconst nothing (make-symbol "nothing"))

(defun -h/contains? (hash key)
  (not (eq nothing (gethash key hash nothing))))

(defun -h/all? (pred hash)
  (let ((res t))
    (maphash (lambda (k v)
               (when res
                 (when (not (funcall pred k v))
                   (setq res nil))))
             hash)
    res))

(defun -h/any? (pred hash)
  (let ((res nil))
    (maphash (lambda (k v)
               (when (not res)
                 (when (funcall pred k v)
                   (setq res t))))
             hash)
    res))

(provide 'dashhash)
