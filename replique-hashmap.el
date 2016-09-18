;;; replique-hashmap.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24"))
;;; Commentary:

;;; Code:

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/hash-map (&rest data)
  (let ((l (length data)))
    (when (not (= 0 (logand l 1)))
      (user-error "Map must contain an even number of forms"))
    (let ((m (make-hash-table :test 'equal))
          (data-rest data))
      (while data-rest
        (puthash (car data-rest) (cadr data-rest) m)
        (setq data-rest (cddr data-rest)))
      m)))

(defun replique/get (hash key &optional default)
  (if (null hash)
      nil
    (gethash key hash default)))

(defun replique/assoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (puthash (car kvs) (cadr kvs) hash)
      (replique/assoc-helper hash (cddr kvs)))))

(defun replique/assoc (hash &rest kvs)
  (let ((args-length (1+ (length kvs))))
    (when (= 0 (logand 1 args-length))
      (user-error
       "replique/assoc expects even number of arguments after hashtable, found odd number"))
    (replique/assoc-helper (copy-hash-table (or hash (replique/hash-map))) kvs)))

(defun replique/dissoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (remhash (car kvs) hash)
      (replique/dissoc-helper hash (cdr kvs)))))

(defun replique/dissoc (hash &rest kvs)
  (replique/dissoc-helper (copy-hash-table (or hash (replique/hash-map))) kvs))

(defconst nothing (make-symbol "nothing"))

(defun replique/contains? (hash key)
  (not (eq nothing (gethash key hash nothing))))

(defun replique/all? (pred hash)
  (let ((res t))
    (maphash (lambda (k v)
               (when res
                 (when (not (funcall pred k v))
                   (setq res nil))))
             hash)
    res))

(defun replique/any? (pred hash)
  (let ((res nil))
    (maphash (lambda (k v)
               (when (not res)
                 (when (funcall pred k v)
                   (setq res t))))
             hash)
    res))

(provide 'replique-hashmap)
