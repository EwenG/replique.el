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

(defvar replique-pprint/line-length nil)
(defvar replique-pprint/line-length-max nil)
(defvar replique-pprint/is-multi-line? nil)

(defun replique-pprint/read-one ()
  (let* ((p (point))
         (p1+ (+ 1 p))
         (p2+ (+ 2 p))
         (p3+ (+ 3 p))
         (char1+ (char-before p1+))
         (char2+ (char-before p2+))
         (char3+ (char-before p3+)))
    (cond ((eq char1+ ?\() :list)
          ((eq char1+ ?\[) :vector)
          ((eq char1+ ?\{) :map)
          ((eq char1+ ?\") :string)
          ((eq ?# char1+)
           (cond ((eq ?: char2+) :namespaced-map)
                 ((and (eq ?? char2+) (eq ?@ char3+)) :reader-conditional-spliced)
                 ((and (eq ?? char2+)) :reader-conditional)
                 ((eq ?# char2+) :symbolic-value)
                 ((eq ?' char2+) :var)
                 ((eq ?_ char2+) :discard)
                 ((eq ?= char2+) :eval)
                 ((eq ?^ char2+) :dispatch-macro-meta)
                 ((eq ?\( char2+) :fn)
                 ((eq ?\{ char2+) :set)
                 ((eq ?\" char2+) :regexp)
                 (t :tagged-literal)))
          ((eq ?^ char1+) :meta)
          ((eq ?~ char1+) (if (eq ?@ char2+) :unquote-splicing :unquote))
          ((or (eq ?' char1+) (eq ?` char1+)) (if (eq ?` char1+) :quote-backtick :quote))
          ((eq ?@ char1+) :deref)
          (t :symbol))))

(defun replique-pprint/pprint-symbol ()
  (setq replique-pprint/is-multi-line? nil)
  (let ((start (point)))
    (skip-chars-forward "^\s,\(\)\[\]\{\}\"\n\t")
    (when (> (point) start)
      (- (point) start))))

(defun replique-pprint/pprint-string ()
  (setq replique-pprint/is-multi-line? nil)
  (let* ((start (point))
         (forward-p (ignore-errors (scan-sexps start 1))))
    (when forward-p
      (goto-char forward-p)
      (- (point) start))))

(defun replique-pprint/indent
    (object-line-length-max object-start object-end indent-length)
  (let ((line-length-before-object replique-pprint/line-length))
    (if replique-pprint/is-multi-line?
        (progn
          (beginning-of-line)
          (setq replique-pprint/line-length (+ (- object-end (point)) indent-length))
          (while (> (point) object-start)
            (insert-char ?\s indent-length t)
            (setq object-end (+ indent-length object-end))
            (forward-line -1))
          (goto-char object-end)
          (setq replique-pprint/line-length-max (max
                                                  replique-pprint/line-length-max
                                                  (+ line-length-before-object
                                                     object-line-length-max))))
      (setq replique-pprint/line-length (+ replique-pprint/line-length object-line-length-max))
      (setq replique-pprint/line-length-max (max replique-pprint/line-length-max
                                                  replique-pprint/line-length)))))

(defun replique-pprint/pprint-sequential (seq)
  (let ((continue t)
        (first-element? t)
        (is-multi-line? nil))
    (let* ((open-delimiter-length (if (eq :set seq) 2 1))
           (replique-pprint/line-length open-delimiter-length)
           (replique-pprint/line-length-max open-delimiter-length))
      (forward-char open-delimiter-length)
      (while continue
        (let ((object-start (point)))
          (replique-context/forward-comment)
          (delete-region object-start (point))
          (let ((object-line-length-max (replique-pprint/pprint)))
            (if (null object-line-length-max)
                (setq continue nil)
              (let ((object-end (point)))
                (cond (first-element?
                       (replique-pprint/indent
                        object-line-length-max object-start object-end open-delimiter-length)
                       (when replique-pprint/is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq is-multi-line? t)
                         (setq replique-pprint/line-length 0))
                       (setq first-element? nil))
                      ((equal replique-pprint/line-length 0)
                       (goto-char object-start)
                       (insert-char ?\s open-delimiter-length t)
                       (setq object-start (+ open-delimiter-length object-start))
                       (setq object-end (+ open-delimiter-length object-end))
                       (setq replique-pprint/line-length open-delimiter-length)
                       (goto-char object-end)
                       (replique-pprint/indent
                        replique-pprint/line-length-max
                        object-start object-end open-delimiter-length)
                       (when replique-pprint/is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq replique-pprint/line-length 0)))
                      ((> (+ replique-pprint/line-length object-line-length-max 1)
                          replique-pprint/threshold)
                       (goto-char object-start)
                       (insert-char ?\n 1 t) (insert-char ?\s open-delimiter-length t)
                       (setq object-start (+ 1 open-delimiter-length object-start))
                       (setq object-end (+ 1 open-delimiter-length object-end))
                       (setq replique-pprint/line-length open-delimiter-length)
                       (goto-char object-end)
                       (replique-pprint/indent
                        object-line-length-max object-start object-end open-delimiter-length)
                       (setq is-multi-line? t)
                       (when replique-pprint/is-multi-line?
                         (setq is-multi-line? t)
                         (insert-char ?\n 1 t)
                         (setq replique-pprint/line-length 0)))
                      (t
                       (goto-char object-start)
                       (insert-char ?\s 1 t)
                       (setq object-start (+ 1 object-start))
                       (setq object-end (+ 1 object-end))
                       (setq replique-pprint/line-length (+ 1 replique-pprint/line-length))
                       (goto-char object-end)
                       (replique-pprint/indent
                        object-line-length-max object-start object-end
                        replique-pprint/line-length)
                       (when replique-pprint/is-multi-line?
                         (insert-char ?\n 1 t)
                         (setq is-multi-line? t)
                         (setq replique-pprint/line-length 0)))))))))
      (when (equal replique-pprint/line-length 0)
        (delete-char -1))
      (setq replique-pprint/is-multi-line? is-multi-line?)
      (forward-char)
      (max replique-pprint/line-length-max (+ replique-pprint/line-length 1)))))

(defun replique-pprint/pprint-map (map)
  (let ((i 0)
        (continue t)
        (replique-pprint/line-length 1)
        (replique-pprint/line-length-max 1)
        (first-element? t)
        (is-multi-line? nil))
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
                     (replique-pprint/indent object-line-length-max object-start object-end 1)
                     (when replique-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq is-multi-line? t)
                       (setq replique-pprint/line-length 0))
                     (setq first-element? nil))
                    ((equal replique-pprint/line-length 0)
                     (goto-char object-start)
                     (insert-char ?\s 1 t)
                     (setq object-start (+ 1 object-start))
                     (setq object-end (+ 1 object-end))
                     (setq replique-pprint/line-length 1)
                     (goto-char object-end)
                     (replique-pprint/indent object-line-length-max object-start object-end 1)
                     (setq is-multi-line? t)
                     (when replique-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq replique-pprint/line-length 0)))
                    ((and
                      (> replique-pprint/line-length replique-pprint/threshold)
                      (> (+ replique-pprint/line-length object-line-length-max 1)
                         replique-pprint/large-threshold))
                     (goto-char object-start)
                     (insert-char ?\n 1 t) (insert-char ?\s 1 t)
                     (setq object-start (+ 2 object-start))
                     (setq object-end (+ 2 object-end))
                     (setq replique-pprint/line-length 1)
                     (goto-char object-end)
                     (replique-pprint/indent
                      object-line-length-max object-start object-end 1)
                     (setq is-multi-line? t)
                     (when replique-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq replique-pprint/line-length 0)))
                    (t
                     (goto-char object-start)
                     (insert-char ?\s 1 t)
                     (setq object-start (+ 1 object-start))
                     (setq object-end (+ 1 object-end))
                     (setq replique-pprint/line-length (+ 1 replique-pprint/line-length))
                     (goto-char object-end)
                     (replique-pprint/indent
                      object-line-length-max object-start object-end replique-pprint/line-length)
                     (when replique-pprint/is-multi-line?
                       (insert-char ?\n 1 t)
                       (setq is-multi-line? t)
                       (setq replique-pprint/line-length 0))))
              (when (and (equal 1 (logand i 1)) (> replique-pprint/line-length 0))
                (insert-char ?\n 1 t)
                (setq replique-pprint/line-length 0))
              (setq i (+ 1 i)))))))
    (when (equal replique-pprint/line-length 0)
      (delete-char -1))
    (setq replique-pprint/is-multi-line? is-multi-line?)
    (forward-char)
    (max replique-pprint/line-length-max (+ replique-pprint/line-length 1))))

(defun replique-pprint/pprint-dispatch-macro (dm)
  (cond ((or
          (eq dm :meta)
          (eq dm :dispatch-macro-meta)
          (eq dm :tagged-literal)
          (eq dm :namespaced-map))
         (let ((dispatch-macro-length (if (eq :dispatch-macro-meta dm) 2 1)))
           (forward-char dispatch-macro-length)
           (let ((data-start (point)))
             (let ((replique-pprint/line-length dispatch-macro-length)
                   (replique-pprint/line-length-max dispatch-macro-length))
               (replique-context/forward-comment)
               ;; Special case for print-level "#" symbol
               (if (and (equal dm :tagged-literal)
                        (> (point) data-start))
                   (progn
                     (goto-char data-start)
                     (setq replique-pprint/is-multi-line? nil)
                     1)
                 (delete-region data-start (point))
                 (let ((data-line-length-max (or (replique-pprint/pprint) 0))
                       (data-end (point))
                       (data-is-multi-line? replique-pprint/is-multi-line?))
                   (replique-pprint/indent
                    data-line-length-max data-start data-end dispatch-macro-length)
                   (let ((data-end (point))
                         (value-start (point)))
                     (replique-context/forward-comment)
                     (delete-region value-start (point))
                     (let* ((value-line-length-max (or (replique-pprint/pprint) 0))
                            (value-end (point))
                            (value-is-multi-line? replique-pprint/is-multi-line?)
                            (break-line? (or
                                          data-is-multi-line? value-is-multi-line?
                                          (and
                                           (> data-line-length-max replique-pprint/threshold)
                                           (> (+ data-line-length-max value-line-length-max 1)
                                              replique-pprint/large-threshold)))))
                       (cond (break-line?
                              (goto-char value-start)
                              (insert-char ?\n 1 t) (insert-char ?\s dispatch-macro-length t)
                              (setq value-start (+ value-start 1 dispatch-macro-length))
                              (setq value-end (+ value-end 1 dispatch-macro-length))
                              (setq replique-pprint/line-length dispatch-macro-length)
                              (setq replique-pprint/is-multi-line? t)
                              (goto-char value-end)
                              (replique-pprint/indent
                               value-line-length-max value-start value-end dispatch-macro-length)
                              (let ((value-end (point)))
                                (goto-char value-start)
                                (let ((next-thing (replique-pprint/read-one)))
                                  (when (or (eq :list next-thing)
                                            (eq :vector next-thing)
                                            (eq :set next-thing)
                                            (eq :map next-thing))
                                    (goto-char value-start)
                                    (let ((delimiter-start (char-after)))
                                      (delete-char 1)
                                      (insert-char ?\s 1 t)
                                      (goto-char value-end)
                                      (let ((delimiter-end (char-before)))
                                        (delete-char -1)
                                        (insert-char ?\n 1 t)
                                        (goto-char data-end)
                                        (insert-char ?\s 1 t)
                                        (insert-char delimiter-start 1 t)
                                        (goto-char (+ 2 value-end))
                                        (insert-char ?\s 1 t)
                                        (insert-char delimiter-end 1 t)))))))
                             ((> value-line-length-max 0)
                              (goto-char value-start)
                              (insert-char ?\s 1 t)
                              (setq value-start (+ 1 value-start))
                              (setq value-end (+ 1 value-end))
                              (setq replique-pprint/line-length
                                    (+ 1 replique-pprint/line-length))
                              (setq replique-pprint/is-multi-line? nil)
                              (goto-char value-end)
                              (replique-pprint/indent
                               value-line-length-max value-start value-end
                               replique-pprint/line-length)))
                       replique-pprint/line-length-max))))))))
        ((or (eq dm :reader-conditional-spliced)
             (eq dm :reader-conditional)
             (eq dm :symbolic-value)
             (eq dm :var)
             (eq dm :discard)
             (eq dm :eval)
             (eq dm :fn)
             (eq dm :regexp))
         (let ((dispatch-macro-length (cond ((or (eq dm :fn) (eq dm :regexp)) 1)
                                            ((eq dm :reader-conditional-spliced) 3)
                                            (t 2))))
           (forward-char dispatch-macro-length)
           (let ((replique-pprint/line-length dispatch-macro-length)
                 (replique-pprint/line-length-max dispatch-macro-length)
                 (object-start (point)))
             (replique-context/forward-comment)
             (delete-region object-start (point))
             (let ((object-line-length-max (or (replique-pprint/pprint) 0)))
               (replique-pprint/indent
                object-line-length-max object-start (point) dispatch-macro-length)
               replique-pprint/line-length-max))))))

(defun replique-pprint/pprint-quoted (q)
  (let ((quote-length (if (eq q :unquote-splicing) 2 1)))
    (let ((replique-pprint/line-length quote-length)
          (replique-pprint/line-length-max quote-length))
      (forward-char quote-length)
      (let* ((object-start (point))
             (object-line-length-max (or (replique-pprint/pprint) 0)))
        (replique-pprint/indent
         object-line-length-max object-start (point) quote-length)
        replique-pprint/line-length-max))))

(defun replique-pprint/pprint-deref (deref)
  (forward-char 1)
  (let ((replique-pprint/line-length 1)
        (replique-pprint/line-length-max 1)
        (object-start (point)))
    (replique-context/forward-comment)
    (delete-region object-start (point))
    (let ((object-line-length-max (or (replique-pprint/pprint) 0)))
      (replique-pprint/indent
       object-line-length-max object-start (point) 1)
      replique-pprint/line-length-max)))

(defun replique-pprint/pprint-dispatch (next-thing)
  (cond ((eq :symbol next-thing) (replique-pprint/pprint-symbol))
        ((eq :string next-thing) (replique-pprint/pprint-string))
        ((or (eq :list next-thing) (eq :vector next-thing) (eq :set next-thing))
         (replique-pprint/pprint-sequential next-thing))
        ((or (eq :map next-thing)) (replique-pprint/pprint-map next-thing))
        ((or (eq :quote next-thing)
             (eq :unquote next-thing)
             (eq :unquote-splicing next-thing)
             (eq :quote-backtick next-thing))
         (replique-pprint/pprint-quoted next-thing))
        ((eq :deref next-thing) (replique-pprint/pprint-deref next-thing))
        (t
         (replique-pprint/pprint-dispatch-macro next-thing))))

(defun replique-pprint/pprint ()
  (when-let (next-thing (replique-pprint/read-one))
    (replique-pprint/pprint-dispatch next-thing)))

(defun replique-pprint/walk-init ()
  (let ((p (point)))
    (if (or (equal ?\) (char-before p))
            (equal ?\} (char-before p))
            (equal ?\] (char-before p)))
        (forward-char -1)
      (skip-chars-backward "^\s,\(\)\[\]\{\}\"\n\t"))
    (if (or (not (equal major-mode 'replique/mode))
            (and (equal (get-char-property (point) 'field) 'output)
                 (>= p (comint-line-beginning-position))))
        (let ((top-level (or (syntax-ppss-toplevel-pos (replique-context/syntax-ppss (point)))
                             (point))))
          (goto-char top-level)
          (let ((object (replique-context/read-one)))
            (if object
                (progn
                  (goto-char top-level)
                  (replique-context/maybe-skip-dispatch-macro-or-quoted-backward)
                  (point))
              (goto-char p)
              nil)))
      (goto-char p)
      nil)))

(defun replique-pprint/pprint** ()
  (let ((replique-context/splice-ends '())
        (replique-pprint/is-multi-line? nil))
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
  (let ((replique-context/platform-tag ":clj"))
    (replique-pprint/pprint*)))

(defun replique-pprint/pprint-cljs (tooling-repl cljs-repl)
  (let ((replique-context/platform-tag ":cljs"))
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
    (let ((replique-context/platform-tag ":clj"))
      (replique-pprint/pprint*))
    (buffer-substring (point-min) (point-max))))

(defun replique-pprint/pprint-error-str (o)
  (let ((replique-pprint/threshold 80))
    (replique-pprint/pprint-str o)))

(provide 'replique-pprint)
