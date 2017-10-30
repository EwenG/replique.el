;; replique-transit.el ---   -*- lexical-binding: t; -*-

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

;; (math-add (math-normalize '(frac (bigpos 213693951 305843009 2) 1)) 2)
;; (symbol-name (read "\\?ee"))

(defclass replique-transit/tagged-value ()
  ((tag :initarg :tag)
   (value :initarg :value)))

(defclass replique-transit/tag ()
  ((tag :initarg :tag)))

(defun replique-transit/decode-list (l)
  (when l
    (let* ((f (car l))
           (rest (cdr l))
           (decoded (replique-transit/decode f)))
      (when (not (eq f decoded))
        (setcar l decoded))
      (replique-transit/decode-list rest))))

(defun replique-transit/decode-map (m)
  (maphash (lambda (k v)
             (let ((k-decoded (replique-transit/decode k))
                   (v-decoded (replique-transit/decode v)))
               (cond ((not (eq k k-decoded))
                      (remhash k m)
                      (puthash k-decoded v-decoded m))
                     ((not (eq v v-decoded))
                      (puthash k-decoded v-decoded m))
                     (t nil))))
           m)
  m)

(defun replique-transit/is-tag (t)
  (cl-typep t 'replique-transit/tag))

(defun replique-transit/decode-array* (a a-length)
  (let ((i 0))
    (while (< i a-length)
      (let* ((element (aref a i))
             (decoded (replique-transit/decode element)))
        (when (not (eq decoded element))
          (aset a i decoded)))
      (setq i (1+ i))))
  a)

(defun replique-transit/decode-array (a)
  (let ((a-length (length a)))
    (if (and (equal a-length 2)
             (stringp (aref a 0)))
        (let ((maybe-tagged-value (replique-transit/decode (aref a 0))))
          (if (replique-transit/is-tag maybe-tagged-value)
              (replique-transit/tagged-value
               :tag maybe-tagged-value
               :value (replique-transit/decode (aref a 1)))
            (replique-transit/decode-array* a a-length)))
      (replique-transit/decode-array* a a-length))))

(defun replique-transit/decode-string (s)
  (if (and (> (length s) 1) (eq ?~ (aref s 0)))
      (cond ((eq ?# (aref s 1)) (replique-transit/tag :tag (substring-no-properties s 2)))
            ((eq ?~ (aref s 1)) (substring-no-properties s 1))
            (t (replique-transit/tagged-value
                :tag (aref s 1) :value (substring-no-properties s 2))))
    s))

(defun replique-transit/decode (node)
  (cond
   ((listp node) (progn (replique-transit/decode-list node) node))
   ((hash-table-p node) (replique-transit/decode-map node))
   ((stringp node) (replique-transit/decode-string node))
   ((arrayp node) (replique-transit/decode-array node))
   (t node)))

(provide 'replique-transit)

(comment
 (replique-transit/decode '(1 ["~#circle" 2] 3))
 (replique-transit/decode ["~#circle" 2])

 (replique-transit/decode (replique/hash-map ["~#circle" [3 4]] 1 :f "~hee"))
 )

