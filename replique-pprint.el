;; replique-pprint.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-context)

(defvar replique-pprint/threshold 20)
(defvar replique-pprint/large-threshold 40)

(defvar line-length nil)
(defvar line-length-max nil)

(defun replique-pprint/pprint-symbol (sym)
  (goto-char (oref sym :end))
  (- (oref sym :end) (oref sym :start)))

(defun replique-pprint/pprint-string (str)
  (goto-char (oref str :end))
  (- (oref str :end) (oref str :start)))

(defun replique-pprint/is-multi-line? (object-start)
  (> (line-beginning-position) object-start))

(defun replique-pprint/indent
    (object-line-length-max object-start object-end is-multi-line? indent-length)
  (let ((line-length-before-object line-length))
    (if is-multi-line?
        (progn
          (beginning-of-line)
          (setq line-length (+ (- object-end (point)) indent-length))
          (while (> (point) object-start)
            (insert-char ?\s indent-length t)
            (setq object-end (+ indent-length object-end))
            (forward-line -1))
          (goto-char object-end)
          (setq line-length-max (max
                                 line-length-max
                                 (+ line-length-before-object object-line-length-max))))
      (setq line-length (+ line-length object-line-length-max))
      (setq line-length-max (max line-length-max line-length)))))

(defun replique-pprint/pprint-sequential (seq)
  (let ((continue t)
        (first-element? t)
        (start (point)))
    (goto-char (+ 1 (oref seq :start)))
    (let* ((open-delimiter-length (- (point) start))
           (line-length open-delimiter-length)
           (line-length-max open-delimiter-length))
      (while continue
        (let ((object-start (point)))
          (replique-context/forward-comment)
          (delete-region object-start (point))
          (let ((object-line-length-max (replique-pprint/pprint)))
            (if (null object-line-length-max)
                (setq continue nil)
              (let ((object-end (point)))
                (cond (first-element?
                       (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                         (replique-pprint/indent
                          object-line-length-max object-start object-end
                          is-multi-line? open-delimiter-length)
                         (when is-multi-line?
                           (insert-char ?\n 1 t)
                           (setq line-length 0)))
                       (setq first-element? nil))
                      ((equal line-length 0)
                       (goto-char object-start)
                       (insert-char ?\s open-delimiter-length t)
                       (setq object-start (+ open-delimiter-length object-start))
                       (setq object-end (+ open-delimiter-length object-end))
                       (setq line-length open-delimiter-length)
                       (goto-char object-end)
                       (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                         (replique-pprint/indent
                          object-line-length-max object-start
                          object-end is-multi-line? open-delimiter-length)
                         (when is-multi-line?
                           (insert-char ?\n 1 t)
                           (setq line-length 0))))
                      ((> (+ line-length object-line-length-max 1) replique-pprint/threshold)
                       (goto-char object-start)
                       (insert-char ?\n 1 t) (insert-char ?\s open-delimiter-length t)
                       (setq object-start (+ 1 open-delimiter-length object-start))
                       (setq object-end (+ 1 open-delimiter-length object-end))
                       (setq line-length open-delimiter-length)
                       (goto-char object-end)
                       (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                         (replique-pprint/indent
                          object-line-length-max object-start object-end
                          is-multi-line? open-delimiter-length)
                         (when is-multi-line?
                           (insert-char ?\n 1 t)
                           (setq line-length 0))))
                      (t
                       (goto-char object-start)
                       (insert-char ?\s 1 t)
                       (setq object-start (+ 1 object-start))
                       (setq object-end (+ 1 object-end))
                       (setq line-length (+ 1 line-length))
                       (goto-char object-end)
                       (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                         (replique-pprint/indent
                          object-line-length-max object-start object-end
                          is-multi-line? line-length)
                         (when is-multi-line?
                           (insert-char ?\n 1 t)
                           (setq line-length 0))))))))))
      (when (equal line-length 0)
        (delete-char -1))
      (forward-char)
      (max line-length-max (+ line-length 1)))))

(defun replique-pprint/pprint-map (map)
  (let ((i 0)
        (continue t)
        (line-length 1)
        (line-length-max 1)
        (first-element? t))
    (forward-char)
    (while continue
      (let ((object-start (point)))
        (replique-context/forward-comment)
        (delete-region object-start (point))
        (let ((object-line-length-max (replique-pprint/pprint)))
          (if (null object-line-length-max)
              (setq continue nil)
            (let ((object-end (point)))
              (cond (first-element?
                     (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                       (replique-pprint/indent
                        object-line-length-max object-start object-end is-multi-line? 1)
                       (when is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq line-length 0)))
                     (setq first-element? nil))
                    ((equal line-length 0)
                     (goto-char object-start)
                     (insert-char ?\s 1 t)
                     (setq object-start (+ 1 object-start))
                     (setq object-end (+ 1 object-end))
                     (setq line-length 1)
                     (goto-char object-end)
                     (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                       (replique-pprint/indent
                        object-line-length-max object-start object-end is-multi-line? 1)
                       (when is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq line-length 0))))
                    ((> (+ line-length object-line-length-max 1) replique-pprint/large-threshold)
                     (goto-char object-start)
                     (insert-char ?\n 1 t) (insert-char ?\s 1 t)
                     (setq object-start (+ 2 object-start))
                     (setq object-end (+ 2 object-end))
                     (setq line-length 1)
                     (goto-char object-end)
                     (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                       (replique-pprint/indent
                        object-line-length-max object-start object-end is-multi-line? 1)
                       (when is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq line-length 0))))
                    (t
                     (goto-char object-start)
                     (insert-char ?\s 1 t)
                     (setq object-start (+ 1 object-start))
                     (setq object-end (+ 1 object-end))
                     (setq line-length (+ 1 line-length))
                     (goto-char object-end)
                     (let ((is-multi-line? (replique-pprint/is-multi-line? object-start)))
                       (replique-pprint/indent
                        object-line-length-max object-start object-end is-multi-line? line-length)
                       (when is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq line-length 0)))))
              (when (and (equal 1 (logand i 1)) (> line-length 0))
                (insert-char ?\n 1 t)
                (setq line-length 0))
              (setq i (+ 1 i)))))))
    (when (equal line-length 0)
      (delete-char -1))
    (forward-char)
    (max line-length-max (+ line-length 1))))

(defun replique-pprint/pprint-dispatch-macro (dm)
  (let ((dispatch-macro (oref dm :dispatch-macro)))
    (cond ((and (equal dispatch-macro :tagged-literal)
                (null (oref dm :data)))
           (forward-char)
           1)
          ((equal dispatch-macro :set)
           (replique-pprint/pprint-sequential (oref dm :value)))
          ((or
            (equal dispatch-macro :meta)
            (equal dispatch-macro :tagged-literal)
            (equal dispatch-macro :namespaced-map))
           (let ((start (point)))
             (forward-char)
             (when (equal ?^ (char-after))
               (forward-char))
             (let* ((data-start (point))
                    (dispatch-macro-length (- data-start start)))
               (let ((line-length dispatch-macro-length)
                     (line-length-max dispatch-macro-length))
                 (replique-context/forward-comment)
                 (delete-region data-start (point))
                 (let ((data-line-length-max (or (replique-pprint/pprint) 0))
                       (data-end (point))
                       (data-is-multi-line? (replique-pprint/is-multi-line? data-start)))
                   (replique-pprint/indent
                    data-line-length-max data-start data-end
                    data-is-multi-line? dispatch-macro-length)
                   (let ((data-end (point))
                         (value-start (point)))
                     (replique-context/forward-comment)
                     (delete-region value-start (point))
                     (let* ((value-line-length-max (or (replique-pprint/pprint) 0))
                            (value-end (point))
                            (value-is-multi-line? (replique-pprint/is-multi-line? value-start))
                            (break-line? (or
                                          data-is-multi-line? value-is-multi-line?
                                          (> (+ data-line-length-max value-line-length-max 1)
                                             replique-pprint/large-threshold))))
                       (cond (break-line?
                              (goto-char value-start)
                              (insert-char ?\n 1 t) (insert-char ?\s dispatch-macro-length t)
                              (setq value-start (+ 1 dispatch-macro-length value-start))
                              (setq value-end (+ 1 dispatch-macro-length value-end))
                              (setq line-length dispatch-macro-length)
                              (goto-char value-end)
                              (replique-pprint/indent
                               value-line-length-max value-start value-end
                               value-is-multi-line? dispatch-macro-length)
                              (goto-char value-start)
                              (let ((object (replique-context/read-one))
                                    (object-end (point)))
                                (when (cl-typep object 'replique-context/object-delimited)
                                  (goto-char (oref object :start))
                                  (let ((delimiter-start (char-after)))
                                    (delete-char 1)
                                    (insert-char ?\s 1 t)
                                    (goto-char (oref object :end))
                                    (let ((delimiter-end (char-before)))
                                      (delete-char -1)
                                      (insert-char ?\n 1 t)
                                      (goto-char data-end)
                                      (insert-char ?\s 1 t)
                                      (insert-char delimiter-start 1 t)
                                      (goto-char (+ 2 (oref object :end)))
                                      (insert-char ?\s 1 t)
                                      (insert-char delimiter-end 1 t))))))
                             ((> value-line-length-max 0)
                              (goto-char value-start)
                              (insert-char ?\s 1 t)
                              (setq value-start (+ 1 value-start))
                              (setq value-end (+ 1 value-end))
                              (setq line-length (+ 1 line-length))
                              (goto-char value-end)
                              (replique-pprint/indent
                               value-line-length-max value-start value-end
                               value-is-multi-line? line-length)))
                       line-length-max)))))))
          ((or (equal dispatch-macro :symbolic-value)
               (equal dispatch-macro :var)
               (equal dispatch-macro :discard)
               (equal dispatch-macro :eval)
               (equal dispatch-macro :fn)
               (equal dispatch-macro :regexp))
           (let ((dispatch-macro-length (if (or (equal dispatch-macro :fn)
                                                (equal dispatch-macro :regexp))
                                            1 2)))
             (forward-char dispatch-macro-length)
             (let ((line-length dispatch-macro-length)
                   (line-length-max dispatch-macro-length)
                   (object-start (point)))
               (replique-context/forward-comment)
               (delete-region object-start (point))
               (let ((object-line-length-max (or (replique-pprint/pprint) 0))
                     (is-multi-line? (replique-pprint/is-multi-line? object-start)))
                 (replique-pprint/indent
                  object-line-length-max object-start (point)
                  is-multi-line? dispatch-macro-length)
                 line-length-max)))))))

(defun replique-pprint/pprint-quoted (q)
  (let ((quote-length (if (oref q :splice?) 2 1)))
    (let ((line-length quote-length)
          (line-length-max quote-length))
      (forward-char quote-length)
      (let* ((object-start (point))
             (object-line-length-max (or (replique-pprint/pprint) 0))
             (is-multi-line? (replique-pprint/is-multi-line? object-start)))
        (replique-pprint/indent
         object-line-length-max object-start (point) is-multi-line? quote-length)
        line-length-max))))

(defun replique-pprint/pprint-deref (deref)
  (forward-char 1)
  (let ((line-length 1)
        (line-length-max 1)
        (object-start (point)))
    (replique-context/forward-comment)
    (delete-region object-start (point))
    (let ((object-line-length-max (or (replique-pprint/pprint) 0))
          (is-multi-line? (replique-pprint/is-multi-line? object-start)))
      (replique-pprint/indent
       object-line-length-max object-start (point)
       is-multi-line? 1)
      line-length-max)))

(defun replique-pprint/pprint-dispatch (object)
  (cond ((cl-typep object 'replique-context/object-symbol)
         (replique-pprint/pprint-symbol object))
        ((cl-typep object 'replique-context/object-string)
         (replique-pprint/pprint-string object))
        ((cl-typep object 'replique-context/object-delimited)
         (if (equal :map (oref object :delimited))
             (replique-pprint/pprint-map object)
           (replique-pprint/pprint-sequential object)))
        ((cl-typep object 'replique-context/object-dispatch-macro)
         (replique-pprint/pprint-dispatch-macro object))
        ((cl-typep object 'replique-context/object-quoted)
         (replique-pprint/pprint-quoted object))
        ((cl-typep object 'replique-context/object-deref)
         (replique-pprint/pprint-deref object))))

(defun replique-pprint/pprint ()
  (let ((p (point)))
    (when-let (object (replique-context/read-one))
      (goto-char p)
      (replique-pprint/pprint-dispatch object))))

(defun replique-pprint/walk-init ()
  (let ((p (point)))
    (when (or (equal ?\) (char-before p))
              (equal ?\} (char-before p))
              (equal ?\] (char-before p)))
      (forward-char -1))
    (if (or (not (equal major-mode 'replique/mode))
            (and (equal (get-char-property (point) 'field) 'output)
                 (>= p (comint-line-beginning-position))))
        (let ((top-level (or (syntax-ppss-toplevel-pos (replique-context/syntax-ppss (point)))
                             (point))))
          (goto-char top-level)
          (let ((object (replique-context/read-one)))
            (if (and object
                     (not (cl-typep object 'replique-context/object-symbol))
                     (not (cl-typep object 'replique-context/object-string)))
                (progn
                  (goto-char top-level)
                  (replique-context/maybe-skip-dispatch-macro-or-quoted-backward)
                  (point))
              (goto-char p)
              nil)))
      (goto-char p)
      nil)))

(defun replique-pprint/pprint** ()
  (let ((replique-context/splice-ends '()))
    (let ((top-level (replique-pprint/walk-init)))
      (when top-level
        (let ((inhibit-field-text-motion t)
              (inhibit-read-only t))
          (when (not (bolp))
            (insert ?\n))
          (replique-pprint/pprint)
          (when (equal major-mode 'replique/mode)
            (put-text-property top-level (point) 'field 'output)))))))

(defun replique-pprint/pprint* ()
  (if font-lock-mode
      (progn
        (font-lock-mode -1)
        (replique-pprint/pprint**)
        (font-lock-mode 1))
    (replique-pprint/pprint**)))

(defun replique-pprint/pprint-clj (tooling-repl clj-repl)
  (let ((replique-context/platform-tag :clj))
    (replique-pprint/pprint*)))

(defun replique-pprint/pprint-cljs (tooling-repl cljs-repl)
  (let ((replique-context/platform-tag :cljs))
    (replique-pprint/pprint*)))

(defun replique-pprint/pprint-cljc (tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((replique-context/platform-tag (symbol-name (replique/get repl :repl-type))))
      (replique-pprint/pprint*))))

(defun replique-pprint/pprint-session (repl)
  (let ((replique-context/platform-tag (symbol-name (replique/get repl :repl-type))))
    (replique-pprint/pprint*)))

(defun replique/pprint ()
  (interactive)
  (replique/with-modes-dispatch
   (replique/mode . 'replique-pprint/pprint-session)
   (clojure-mode . 'replique-pprint/pprint-clj)
   (clojurescript-mode . 'replique-pprint/pprint-cljs)
   (clojurec-mode . 'replique-pprint/pprint-cljc)))

(defun replique-pprint/pprint-str (o)
  (with-temp-buffer
    (replique-print/print o)
    (let ((replique-context/platform-tag :clj))
      (replique-pprint/pprint*))
    (buffer-substring (point-min) (point-max))))

(defun replique-pprint/pprint-error-str (o)
  (let ((replique-pprint/threshold 80))
    (replique-pprint/pprint-str o)))

(provide 'replique-pprint)

