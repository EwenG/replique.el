;; replique-hashmap.el ---   -*- lexical-binding: t; -*-

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

(defun replique/hash-map (&rest data)
  (let ((l (length data)))
    (when (not (= 0 (logand l 1)))
      (error "Map must contain an even number of forms"))
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

(defun replique/get-in-helper (hash ks default-val)
  (let ((k (car ks))
        (ks-rest (cdr ks)))
    (if (or (null ks-rest) (null hash))
        (replique/get hash k default-val)
      (replique/get-in-helper (replique/get hash k) ks-rest default-val))))

(defun replique/get-in (hash ks &optional default-val)
  (if (equal 0 (length ks))
      hash
    (replique/get-in-helper data (append ks '()) default-val)))

(defun replique/copy-hash-table (hash)
  (if (hash-table-p hash)
      (copy-hash-table hash)
    (replique/hash-map)))

(defun replique/assoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (puthash (car kvs) (cadr kvs) hash)
      (replique/assoc-helper hash (cddr kvs)))))

(defun replique/assoc (hash &rest kvs)
  (let ((args-length (1+ (length kvs))))
    (when (= 0 (logand 1 args-length))
      (error
       "replique/assoc expects even number of arguments after hashtable, found odd number"))
    (replique/assoc-helper (replique/copy-hash-table hash) kvs)))

(defun replique/assoc-in-helper (hash ks v ks-length-dec ks-index)
  (let ((copy (replique/copy-hash-table hash)))
    (if (equal ks-index ks-length-dec)
        (progn (puthash (elt ks ks-index) v copy)
               copy)
      (progn (puthash (elt ks ks-index)
                      (replique/assoc-in-helper
                       (gethash (elt ks ks-index) copy) ks v ks-length-dec (+ 1 ks-index)) 
                      copy)
               copy))))

(defun replique/assoc-in (hash ks v)
  (let ((ks-length (length ks)))
    (if (equal 0 ks-length)
        (replique/copy-hash-table hash)
      (replique/assoc-in-helper hash ks v (- ks-length 1) 0))))

(defun replique/dissoc-helper (hash kvs)
  (if (null kvs)
      hash
    (progn
      (remhash (car kvs) hash)
      (replique/dissoc-helper hash (cdr kvs)))))

(defun replique/dissoc (hash &rest kvs)
  (replique/dissoc-helper (replique/copy-hash-table hash) kvs))

(defun replique/update-in-helper (hash ks f args)
  (let ((k (car ks))
        (k-rest (cdr ks)))
    (if (null k-rest)
        (replique/assoc hash k (apply f (replique/get hash k) args))
      (replique/assoc
       hash k
       (replique/update-in-helper (replique/get hash k) k-rest f args)))))

(defun replique/update-in (hash ks f &rest args)
  (let ((ks (append ks '())))
    (if (equal 0 (length ks))
        (replique/copy-hash-table hash)
      (replique/update-in-helper hash ks f args))))

(defconst replique/nothing (make-symbol "nothing"))

(defun replique/contains? (hash key)
  (not (eq replique/nothing (gethash key hash replique/nothing))))

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

;; Not really hash-map specific ...
(defun replique/conj (coll x &rest xs)
  (cond ((listp coll) (if (null xs)
                          (cons x coll)
                        (apply 'replique/conj (cons x coll) (car xs) (cdr xs))))
        ((arrayp coll) (vconcat coll `[,x] xs))
        ;; conj to hash-tables not yet implemented
        (t (error "cannot conj to %s" coll))))

(provide 'replique-hashmap)
