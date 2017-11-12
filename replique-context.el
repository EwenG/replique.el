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

(defun replique-context/state-depth (parser-state)
  (car parser-state))

(defun replique-context/quote-char ()
  (let ((char-b (char-before (point))))
    (when (or (eq ?' char-b) (eq ?` char-b))
      char-b)))

(defun replique-context/after-unquote? ()
  (let ((char1- (char-before (point)))
        (char2- (char-before (1- (point)))))
    (or (eq ?~ char1-) (and (eq ?~ char2-) (eq ?@ char1-)))))

(defun replique-context/init-state ()
  (let* ((p (point))
         (parser-state (syntax-ppss p)))
    (replique/hash-map
     :point p
     :depth (replique-context/state-depth parser-state)
     :toplevel (syntax-ppss-toplevel-pos parser-state))))

(defvar replique-context/parser-state nil)
(defvar replique-context/context-state nil)

(defun replique-context/at-vector? (forward-p)
  (and (eq (char-after (point)) ?\[)
       (eq (char-after forward-p) ?\])))

(defun replique-context/maybe-at-function (forward-p)
  (let ((quoted? (replique/get replique-context/context-state :quoted)))
    (if (and (or (and (not quoted?) (null (replique-context/quote-char)))
                 (and quoted? (replique-context/after-unquote?)))
             (eq (char-after (point)) ?\()
             (eq (char-after forward-p) ?\)))
        (progn
          (parse-partial-sexp (point) forward-p 1 nil)
          (parse-partial-sexp (point) forward-p nil t)
          t)
      (goto-char (1+ forward-p))
      nil)))

(defun replique-context/symbol-at (from to &optional quote-char)
  (let ((symbol-s (buffer-substring-no-properties from to)))
    (if quote-char
        (make-symbol (concat (string quote-char) symbol-s))
      (make-symbol symbol-s))))

(defun replique-context/maybe-at-symbol (forward-p symbol)
  (let* ((quote-char (replique-context/quote-char))
         (before-p (point))
         (after-p (ignore-errors (scan-sexps before-p 1))))
    (if after-p
        (if symbol
            (if (equal (symbol-name (replique-context/symbol-at before-p after-p quote-char))
                       (symbol-name symbol))
                (progn (goto-char after-p)
                       (parse-partial-sexp (point) forward-p nil t)
                       symbol)
              (goto-char (1+ forward-p))
              nil)
          (goto-char after-p)
          (parse-partial-sexp (point) forward-p nil t)
          (replique-context/symbol-at before-p after-p quote-char))
      (goto-char (1+ forward-p))
      nil)))

(defun replique-context/maybe-at-in-ns (forward-p)
  (when (replique-context/maybe-at-function forward-p)
    (when (replique-context/maybe-at-symbol forward-p 'in-ns)
      (let ((namespace (replique-context/maybe-at-symbol forward-p nil)))
        (if namespace
            (progn
              (goto-char (1+ forward-p))
              namespace)
          (goto-char (1+ forward-p))
          nil)))))

(comment
 (in-ns 'rr)
 (let ((forward-p (ignore-errors (scan-sexps (point) 1))))
   (when forward-p
     (replique-context/maybe-at-in-ns (1- forward-p))))
 )

(defun replique-context/update-quoted-state ()
  (let ((quote-char (replique-context/quote-char)))
    (cond ((replique-context/after-unquote?)
           (puthash :quoted nil replique-context/context-state))
          ((or (eq ?' quote-char) (eq ?` quote-char))
           (puthash :quoted t replique-context/context-state))
          (t nil))))

(defun replique-context/walk-in-ns (target-point)
  (while (< (point) target-point)
    (let* ((forward-point (ignore-errors (scan-sexps (point) 1))))
      (if forward-point
          (if (< forward-point target-point)
              (let ((namespace (replique-context/maybe-at-in-ns (1- forward-point))))
                (when namespace
                  (puthash :namespace namespace replique-context/context-state))
                (parse-partial-sexp (point) target-point nil t))
            (replique-context/update-quoted-state)
            (parse-partial-sexp (point) target-point 1 nil)
            (parse-partial-sexp (point) target-point nil t))
        (goto-char target-point))))
  replique-context/context-state)

(defun replique-context/walk (target-point)
  (parse-partial-sexp (point) target-point nil t)
  (if (>= (point) target-point)
      replique-context/context-state
    (let* ((forward-point (scan-sexps (point) 1)))
      (if (< forward-point target-point)
          (cond ((and (equal :function (replique/get replique-context/context-state :wrapper))
                      (equal 0 (replique/get replique-context/context-state :position)))
                 t)
                ((and (equal :function (replique/get replique-context/context-state :wrapper))
                      (replique-context/at-vector? forward-point))
                 t))
        nil))))

(defun replique-context/walk-init ()
  (let* ((target-point (point))
         (top-level (syntax-ppss-toplevel-pos (syntax-ppss target-point))))
    (when top-level
      (goto-char top-level)
      (parse-partial-sexp (point) target-point nil t)
      (setq replique-context/parser-state nil)
      (setq replique-context/context-state (replique/hash-map))
      target-point)))

(comment
 (let* ((forward-point (replique-context/walk-init))
        (context-state (replique-context/walk-in-ns forward-point)))
   (when context-state
     (replique/get context-state :namespace)))
 )

;; clojure-find-ns does not consider (in-ns ...) forms that don't start at column 0, for
;; example (in-ns ...) forms that are in a (comment ...) block
(defun replique-context/clojure-find-ns (tooling-repl repl-env ns)
  (let ((tooling-chan (replique/get tooling-repl :chan)))
    (replique/send-tooling-msg
     tooling-repl
     (replique/hash-map :type :context
                        :repl-env repl-env
                        :ns ns
                        :contexts [:in-ns]))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (if err
               (progn
                 (message "%s" (replique-edn/pr-str err))
                 (message "context failed"))
             (print (replique-edn/pr-str resp)))))))))

(comment
 (let ((tooling-repl (replique/active-repl :tooling t)))
   (replique-context/clojure-find-ns tooling-repl :replique/clj "replique.tooling"))
 )

(provide 'replique-context)
