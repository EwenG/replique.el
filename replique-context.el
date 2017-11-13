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

(defvar replique-context/parser-state nil)
(defvar replique-context/context-state nil)

(defvar replique-context/in-ns-forms '(in-ns clojure.core/in-ns))

(defun replique-context/init-state ()
  (setq replique-context/parser-state nil)
  (setq replique-context/context-state (replique/hash-map)))

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

(defun replique-context/after-read-comment? ()
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

(defun replique-context/at-delimited? (forward-p open close)
  (and (eq (char-after (point)) open)
       (eq (char-before forward-p) close)))

(defun replique-context/at-vector? (forward-p)
  (replique-context/maybe-at-delimited forward-p ?\[ ?\]))

(defun replique-context/symbol-at (from to &optional quote-char)
  (let ((symbol-s (buffer-substring-no-properties from to)))
    (if quote-char
        (make-symbol (concat (string quote-char) symbol-s))
      (make-symbol symbol-s))))

(defun replique-context/maybe-at-dispatch-macro (forward-p)
  ;; We already have read the dispatch macro, it will be cleared by the next read
  (when (and (null (replique/get replique-context/context-state :dispatch-macro))
             (null (replique/get replique-context/context-state :quoted)))
    (let ((char1- (char-before (point)))
          (char2- (char-before (1- (point))))
          (char3- (char-before (- (point) 2)))
          (char1+ (char-after (point))))
      (cond ((and (eq ?# char1-) (eq ?: char1+))
             (goto-char forward-p)
             :namespaced-map)
            ((or (and (eq ?# char2-) (eq ?? char1-))
                 (and (eq ?# char3-) (eq ?? char2-) (eq ?@ char1-)))
             :reader-conditional)
            ((and (eq ?# char2-) (eq ?# char1-))
             :symbolic-value)
            ((and (eq ?# char2-) (eq ?' char1-))
             :var)
            ((and (eq ?# char1-) (eq ?_ char1+))
             (goto-char forward-p)
             :discard)
            ((and (eq ?# char1-) (eq ?= char1+))
             (goto-char forward-p)
             :eval)
            ((eq ?^ char1-)
             :meta)
            ((and (eq ?# char1-) (replique-context/at-delimited? forward-p ?\( ?\)))
             :fn)
            ((and (eq ?# char1-) (replique-context/at-delimited? forward-p ?\{ ?\}))
             :set)
            ((and (eq ?# char1-) (replique-context/at-delimited? forward-p ?\< ?\>))
             :throwing)
            ((and (eq ?# char1-) (replique-context/at-delimited? forward-p ?\" ?\"))
             :regex)
            ((and (eq ?# char1-) (replique-context/at-delimited? forward-p ?\[ ?\]))
             nil)
            (t nil)))))

(defun replique-context/maybe-at-quoted (forward-p)
  ;; We already have read the quoted-state, it will be cleared by the next read
  (when (and (null (replique/get replique-context/context-state :quoted))
             (null (replique/get replique-context/context-state :dispatch-macro)))
    (let ((char1- (char-before (point)))
          (char2- (char-before (1- (point)))))
      (cond ((or (and (eq ?~ char2-) (eq ?@ char1-))
                 (eq ?~ char1-))
             :unquote)
            ((or (eq ?' char1-) (eq ?` char1-))
             :quote)
            (t nil)))))

;; #: #:: #::alias -> namespaced map
;; #?() #?@() -> reader conditional
;; ## -> symbolic value
;; #' -> var
;; #_ -> discard
;; #= eval
;; #< throwing
;; #^ -> meta
;; ^ -> meta
;; #() -> anonymous fn
;; #{} -> set
;; #"" -> regexp

(comment
 (let ((forward-p (ignore-errors (scan-sexps (point) 1))))
   (when forward-p
     (replique-context/init-state)
     (replique-context/maybe-at-dispatch-macro forward-p)))
 )

(defun replique-context/maybe-at-symbol (forward-p symbol)
  (let* ((quote-char (replique-context/quote-char))
         (before-p (point))
         (after-p (ignore-errors (scan-sexps before-p 1))))
    (if after-p
        (if symbol
            (let ((symbol-at-p (replique-context/symbol-at before-p after-p quote-char)))
              (if (if (consp symbol)
                      (seq-find (lambda (sym)
                                  (equal (symbol-name symbol-at-p) (symbol-name sym)))
                                symbol)
                    (equal (symbol-name symbol-at-p) (symbol-name symbol)))
                  (progn (goto-char after-p)
                         (parse-partial-sexp (point) forward-p nil t)
                         symbol-at-p)
                (goto-char (1+ forward-p))
                nil))
          (goto-char after-p)
          (parse-partial-sexp (point) forward-p nil t)
          (replique-context/symbol-at before-p after-p quote-char))
      (goto-char (1+ forward-p))
      nil)))

(defun replique-context/maybe-at-function (forward-p)
  (let ((dispatch-macro (replique/get replique-context/context-state :dispatch-macro))
        (local-quoted (replique/get replique-context/context-state :quoted))
        (global-quoted (replique/get replique-context/context-state :global-quoted)))
    (if (and (null dispatch-macro)
             (or (equal local-quoted :unquote)
                 (and (null local-quoted) (null global-quoted)))
             (eq (char-after (point)) ?\()
             (eq (char-after forward-p) ?\)))
        (progn
          (parse-partial-sexp (point) forward-p 1 nil)
          (parse-partial-sexp (point) forward-p nil t)
          t)
      (goto-char (1+ forward-p))
      nil)))

(defun replique-context/maybe-at-in-ns (forward-p)
  (when (replique-context/maybe-at-function forward-p)
    (when (replique-context/maybe-at-symbol forward-p replique-context/in-ns-forms)
      (let ((namespace (replique-context/maybe-at-symbol forward-p nil)))
        (if namespace
            (progn
              (goto-char (1+ forward-p))
              namespace)
          (goto-char (1+ forward-p))
          nil)))))

(comment
 (let ((forward-p (ignore-errors (scan-sexps (point) 1))))
   (replique-context/init-state)
   (when forward-p
     (replique-context/maybe-at-in-ns (1- forward-p))))
 )

(defun replique-context/update-global-quoted-state ()
  (let ((local-quoted (replique/get replique-context/context-state :quoted)))
    (cond ((equal local-quoted :quote)
           (puthash :global-quoted t replique-context/context-state))
          ((equal local-quoted :unquote)
           (puthash :global-quoted nil replique-context/context-state))
          (t nil))))

(defun replique-context/walk-in-ns (target-point)
  (while (< (point) target-point)
    (let* ((forward-point (ignore-errors (scan-sexps (point) 1))))
      (if forward-point
          (let* ((dispatch-macro (replique-context/maybe-at-dispatch-macro forward-point))
                 (quoted (replique-context/maybe-at-quoted forward-point)))
            (cond (dispatch-macro
                   (puthash :dispatch-macro dispatch-macro replique-context/context-state))
                  (quoted
                   (puthash :quoted quoted replique-context/context-state))
                  (t
                   (let ((before-target? (< forward-point target-point)))
                     (if before-target?
                         (let ((namespace (replique-context/maybe-at-in-ns (1- forward-point))))
                           (when namespace
                             (puthash :namespace namespace replique-context/context-state)))
                       (replique-context/update-global-quoted-state)
                       (parse-partial-sexp (point) target-point 1 nil))
                     (puthash :dispatch-macro nil replique-context/context-state)
                     (puthash :quoted nil replique-context/context-state))))
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
      (replique-context/init-state)
      target-point)))

(comment
 (let* ((forward-point (replique-context/walk-init))
        (context-state (replique-context/walk-in-ns forward-point)))
   (when context-state
     (replique/get context-state :namespace)))
 )

;; clojure-find-ns does not consider (in-ns ...) forms that don't start at column 0, for
;; example (in-ns ...) forms that are in a (comment ...) block
(defun replique-context/clojure-find-ns ()
  (let ((forward-point (replique-context/walk-init)))
    (when forward-point
      (let ((context-state (replique-context/walk-in-ns forward-point)))
        (when context-state
          (replique/get context-state :namespace))))))

(comment
 (let ((tooling-repl (replique/active-repl :tooling t)))
   (replique-context/clojure-find-ns tooling-repl :replique/clj "replique.tooling"))
 )

(provide 'replique-context)


(comment
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
            (print (replique-edn/pr-str resp))))))))
 )
