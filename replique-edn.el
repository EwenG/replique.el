;;; replique-edn.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'eieio)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-edn/write-string* (str)
  (let ((idx 0)
        (chars nil))
    (while (< idx (length str))
      (let ((ch (elt str idx)))
        (cond ((or (equal ?\\ ch)
                   (equal ?\" ch))
               (push ?\\ chars)
               (push ch chars))
              (t (push ch chars))))
      (setq idx (1+ idx)))
    (format "\"%s\""
            (apply 'string (reverse chars)))))

(defclass replique-edn/printable ()
  ()
  :abstract t)

(defmethod replique-edn/print-method ((o replique-edn/printable))
  (let ((slots (mapcar (lambda (s)
                         (intern (concat ":" (symbol-name s))))
                       (object-slots o)))
        (l nil))
    (mapcar (lambda (s)
              (push (slot-value o s) l)
              (push s l))
            slots)
    (format "#%s %s"
            (object-class o)
            (replique-edn/pr-str
             (replique-edn/list-to-map l)))))

(defun replique-edn/pr-str (data)
  (cond ((null data) "nil")
        ((equal t data) "true")
        ((replique-edn/printable-child-p data)
         (replique-edn/print-method data))
        ((numberp data) (format "%s" data))
        ((stringp data) (replique-edn/write-string* data))
        ((symbolp data) (format "%s" data))
        ((vectorp data)
         (format
          "[%s]"
          (s-join " " (mapcar 'replique-edn/pr-str data))))
        ((hash-table-p data)
         (let ((l nil))
           (maphash
            (lambda (x y)
              (push x l)
              (push y l))
            data)
           (format
            "{%s}"
            (s-join
             " "
             (mapcar 'replique-edn/pr-str (reverse l))))))
        ((listp data)
         (format
          "(%s)"
          (s-join " " (mapcar 'replique-edn/pr-str data))))
        (t (error "%s cannot be printed to EDN." data))))

(provide 'replique-edn)

;;; replique-edn.el ends here
