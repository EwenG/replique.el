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

(defvar replique-context/forward-context nil)
(defvar replique-context/dispatch-macro nil)
(defvar replique-context/quoted nil)
(defvar replique-context/global-quoted nil)
(defvar replique-context/namespace nil)

(defvar replique-context/in-ns-forms '(in-ns clojure.core/in-ns))

(defun replique-context/init-state ()
  (setq replique-context/forward-context nil)
  (setq replique-context/dispatch-macro nil)
  (setq replique-context/quoted nil)
  (setq replique-context/global-quoted nil)
  (setq replique-context/namespace nil))

(defun replique-context/delimited? (open close from &optional to)
  (let ((to (or to (ignore-errors (scan-sexps from 1)))))
    (when to
      (and (eq (char-after from) open)
           (eq (char-before to) close)))))

(defun replique-context/forward-comment ()
   (forward-comment (buffer-size))
   (while (> (skip-chars-forward ",") 0)
     (forward-comment (buffer-size))))

;; # followed more than one whitespaces are ignored even though this is valid clojure
(defun replique-context/maybe-at-dispatch-macro ()
  (let* ((p (point))
         (p1+ (+ 1 p))
         (p2+ (+ 2 p))
         (p3+ (+ 3 p))
         (char1+ (char-before p1+))
         (char2+ (char-before p2+))
         (char3+ (char-before p3+)))
    (cond ((eq ?# char1+)
           (cond ((eq ?: char2+)
                  (setq replique-context/dispatch-macro :namespaced-map)
                  p1+)
                 ((and (eq ?? char2+) (eq ?@ char3+) (replique-context/delimited? ?\( ?\) p3+))
                  (setq replique-context/dispatch-macro :reader-conditional)
                  p3+)
                 ((and (eq ?? char2+) (replique-context/delimited? ?\( ?\) p2+))
                  (setq replique-context/dispatch-macro :reader-conditional)
                  p2+)
                 ;; allow zero or multiple spaces after the symbolic value reader
                 ((eq ?# char2+)
                  (goto-char p2+)
                  (replique-context/forward-comment)
                  (let ((forward-p (point)))
                    (goto-char p)
                    (setq replique-context/dispatch-macro :symbolic-value)
                    forward-p))
                 ((eq ?' char2+)
                  (setq replique-context/dispatch-macro :var)
                  p2+)
                 ((eq ?_ char2+)
                  (setq replique-context/dispatch-macro :discard)
                  p2+)
                 ((eq ?= char2+)
                  (setq replique-context/dispatch-macro :eval)
                  p2+)
                 ((eq ?^ char2+)
                  (setq replique-context/dispatch-macro :meta)
                  p2+)
                 ((replique-context/delimited? ?\( ?\) p1+)
                  (setq replique-context/dispatch-macro :fn)
                  p1+)
                 ((replique-context/delimited? ?\{ ?\} p1+)
                  (setq replique-context/dispatch-macro :set)
                  p1+)
                 ((replique-context/delimited? ?\" ?\" p1+)
                  (setq replique-context/dispatch-macro :regex)
                  p1+)
                 ;; allow zero or multiple spaces after the tagged literal value reader
                 (t (goto-char p1+)
                    (replique-context/forward-comment)
                    (let ((forward-p (point)))
                      (goto-char p)
                      (setq replique-context/dispatch-macro :tagged-literal)
                      forward-p))))
          ((eq ?^ char1+)
           (setq replique-context/dispatch-macro :meta)
           p1+)
          (t nil))))

(defun replique-context/maybe-at-quoted ()
  (let* ((p (point))
         (p1+ (+ 1 p))
         (p2+ (+ 2 p))
         (char1+ (char-before p1+))
         (char2+ (char-before p2+)))
    (if (eq ?~ char1+)
        (if (eq ?@ char2+)
            (progn
              (setq replique-context/quoted :unquote)
              p2+)
          (setq replique-context/quoted :unquote)
          p1+)
      (when (or (eq ?' char1+) (eq ?` char1+))
        (setq replique-context/quoted :quote)
        p1+))))

;; #: #:: #::alias -> namespaced map
;; #?() #?@() -> reader conditional
;; ## -> symbolic value
;; #' -> var
;; #_ -> discard
;; #= eval
;; #< throwing -- here considered as a tag reader
;; #^ -> meta
;; ^ -> meta
;; #() -> anonymous fn
;; #{} -> set
;; #"" -> regexp

(comment
 (replique-context/maybe-at-dispatch-macro)
 )

(defun replique-context/scan-forward ()
  (let* ((p (point))
         (char1+ (char-after (point))))
    (cond ((eq char1+ ?\()
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\) (char-before forward-p)))
               (setq replique-context/forward-context :list)
               forward-p)))
          ((eq char1+ ?\[)
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\] (char-before forward-p)))
               (setq replique-context/forward-context :vector)
               forward-p)))
          ((eq char1+ ?\{)
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\} (char-before forward-p)))
               (setq replique-context/forward-context :map)
               forward-p)))
          ((eq char1+ ?\")
           (let ((forward-p (ignore-errors (scan-sexps p 1))))
             (when (and forward-p (eq ?\" (char-before forward-p)))
               (setq replique-context/forward-context :string)
               forward-p)))
          (t (let ((forward-dispatch-macro (replique-context/maybe-at-dispatch-macro)))
               (if forward-dispatch-macro
                   (progn
                     (setq replique-context/forward-context :dispatch-macro)
                     forward-dispatch-macro)
                 (let ((forward-quoted (replique-context/maybe-at-quoted)))
                   (if forward-quoted
                       (progn
                         (setq replique-context/forward-context :quoted)
                         forward-quoted)
                     (setq replique-context/forward-context :symbol)
                     (skip-chars-forward "^[\s,\(\)\[\]\{\}\"\n\t]*")
                     (let ((forward-p (point)))
                       (goto-char p)
                       forward-p)))))))))

(defun replique-context/delimited-forward-context? ()
  (or (eq :list replique-context/forward-context)
      (eq :vector replique-context/forward-context)
      (eq :map replique-context/forward-context)))

(defmacro replique-context/scan-forward-with-quote (forward-point-sym &rest body)
  `(let ((,forward-point-sym (replique-context/scan-forward)))
     (if (and ,forward-point-sym
              (eq :quoted replique-context/forward-context)
              (eq :quote replique-context/quoted))
         (progn
           (goto-char ,forward-point-sym)
           (let ((,forward-point-sym (replique-context/scan-forward)))
             ,@body))
       (setq replique-context/quoted nil)
       ,@body)))

(defun replique-context/maybe-at-in-ns (end-point)
  (when (and (or (null replique-context/dispatch-macro)
                 (eq replique-context/dispatch-macro :tagged-literal))
             (or (equal replique-context/quoted :unquote)
                 (and (null replique-context/quoted)
                      (null replique-context/global-quoted))))
    (forward-char)
    (replique-context/forward-comment)
    (let ((forward-point (replique-context/scan-forward)))
      (when (and forward-point (eq :symbol replique-context/forward-context))
        (let ((symbol-at-p (buffer-substring-no-properties (point) forward-point)))
          (when (seq-find (lambda (sym)
                            (equal symbol-at-p (symbol-name sym)))
                          replique-context/in-ns-forms)
            (goto-char forward-point)
            (replique-context/forward-comment)
            (replique-context/scan-forward-with-quote
             forward-point
             (let ((quote-char (when (eq :quote replique-context/quoted)
                                 (char-before (point)))))
               (when (and forward-point
                          (eq :symbol replique-context/forward-context)
                          (not (eq (point) forward-point)))
                 (let ((symbol-at-point (buffer-substring-no-properties
                                         (point) forward-point)))
                   (if quote-char
                       (make-symbol (concat (string quote-char) symbol-at-point))
                     (make-symbol symbol-at-point))))))))))))

(comment
 (let ((forward-p (ignore-errors (scan-sexps (point) 1))))
   (replique-context/init-state)
   (when forward-p
     (replique-context/maybe-at-in-ns (1- forward-p))))
 )

(defun replique-context/update-global-quoted-state ()
  (cond ((equal replique-context/quoted :quote)
         (setq replique-context/global-quoted t))
        ((equal replique-context/quoted :unquote)
         (setq replique-context/global-quoted nil))
        (t nil)))

(defun replique-context/walk-in-ns (target-point)
  (while (< (point) target-point)
    (let* ((forward-point (replique-context/scan-forward)))
      (if forward-point
          (cond ((replique-context/delimited-forward-context?)
                 (if (< (1- forward-point) target-point)
                     (progn
                       ;; Before target point
                       (when (eq :list replique-context/forward-context)
                         (let ((namespace (replique-context/maybe-at-in-ns (1- forward-point))))
                           (when namespace
                             (setq replique-context/namespace namespace))))
                       ;; Before target point but not a list
                       (goto-char forward-point))
                   ;; After target point
                   (replique-context/update-global-quoted-state)
                   (forward-char))
                 (setq replique-context/dispatch-macro nil)
                 (setq replique-context/quoted nil)
                 (replique-context/forward-comment))
                ((eq :string replique-context/forward-context)
                 (goto-char forward-point)
                 (setq replique-context/dispatch-macro nil)
                 (setq replique-context/quoted nil)
                 (replique-context/forward-comment))
                ((eq :dispatch-macro replique-context/forward-context)
                 (goto-char forward-point))
                ((eq :quoted replique-context/forward-context)
                 ;; clear the dispatch-macro
                 (setq replique-context/dispatch-macro nil)
                 (goto-char forward-point))
                ((eq :symbol replique-context/forward-context)
                 (setq replique-context/dispatch-macro nil)
                 (setq replique-context/quoted nil)
                 (goto-char forward-point)
                 (replique-context/forward-comment)))
        (goto-char target-point))))
  replique-context/namespace)

(defun replique-context/walk-init ()
  (let* ((target-point (point))
         (top-level (syntax-ppss-toplevel-pos (syntax-ppss target-point))))
    (when top-level
      (goto-char top-level)
      (skip-chars-backward "^[\s,\(\)\[\]\{\}\"\n\t]*")
      (replique-context/forward-comment)
      (replique-context/init-state)
      target-point)))

(comment
 (let* ((forward-point (replique-context/walk-init)))
   (replique-context/walk-in-ns forward-point))
 )

;; find in-ns calls in the current form. If no namespace is found, use clojure-find-ns
(defun replique-context/clojure-find-ns ()
  (save-excursion
    (let* ((forward-point (replique-context/walk-init))
           (namespace (when forward-point (replique-context/walk-in-ns forward-point))))
      (or namespace (clojure-find-ns)))))

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
