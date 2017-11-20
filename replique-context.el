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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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
                  (skip-chars-forward "^[\s,\(\)\[\]\{\}\"\n\t]*")
                  (point))
                 ((and (eq ?? char2+) (eq ?@ char3+))
                  (setq replique-context/dispatch-macro :reader-conditional)
                  p3+)
                 ((and (eq ?? char2+))
                  (setq replique-context/dispatch-macro :reader-conditional)
                  p2+)
                 ((eq ?# char2+)
                  (setq replique-context/dispatch-macro :symbolic-value)
                  p2+)
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
                 (t (goto-char p1+)
                    (setq replique-context/dispatch-macro :tagged-literal)
                    p1+)))
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

(defclass replique-context/object-dispatch-macro ()
  ((dispatch-macro :initarg :dispatch-macro)
   (data :initarg :data)
   (value :initarg :value)))

(defclass replique-context/object-quoted ()
  ((quoted :initarg :quoted)
   (value :initarg :value)))

(defclass replique-context/object-delimited ()
  ((delimited :initarg :delimited)
   (start :initarg :start)
   (end :initarg :end)))

(defclass replique-context/object-string ()
  ((string :initarg :string)
   (start :initarg :start)
   (end :initarg :end)))

(defclass replique-context/object-symbol ()
  ((symbol :initarg :symbol)
   (start :initarg :start)
   (end :initarg :end)))

(defun replique-context/read-one ()
  (let ((forward-point (replique-context/scan-forward)))
    (when forward-point
      (cond ((eq :dispatch-macro replique-context/forward-context)
             (cond ((eq :discard replique-context/dispatch-macro)
                    (goto-char forward-point)
                    (replique-context/forward-comment)
                    (replique-context/read-one)
                    (replique-context/forward-comment)
                    (replique-context/read-one))
                   ((or (eq :meta replique-context/dispatch-macro)
                        (eq :tagged-literal replique-context/dispatch-macro))
                    (goto-char forward-point)
                    (replique-context/forward-comment)
                    (let ((dispatch-macro replique-context/dispatch-macro)
                          (data (replique-context/read-one))
                          (_ (replique-context/forward-comment))
                          (value (replique-context/read-one)))
                      (replique-context/object-dispatch-macro
                       :dispatch-macro dispatch-macro
                       :data data
                       :value value)))
                   ((or (eq :eval replique-context/dispatch-macro)
                        (eq :fn replique-context/dispatch-macro)
                        (eq :set replique-context/dispatch-macro)
                        (eq :regexp replique-context/dispatch-macro))
                    (goto-char forward-point)
                    (replique-context/object-dispatch-macro
                     :dispatch-macro replique-context/dispatch-macro
                     :data nil
                     :value (replique-context/read-one)))
                   (t
                    (goto-char forward-point)
                    (replique-context/forward-comment)
                    (replique-context/object-dispatch-macro
                     :dispatch-macro replique-context/dispatch-macro
                     :data nil
                     :value (replique-context/read-one)))))
            ((eq :quoted replique-context/forward-context)
             (goto-char forward-point)
             (replique-context/object-quoted
              :quoted replique-context/quoted
              :value (replique-context/read-one)))
            ((replique-context/delimited-forward-context?)
             (let ((start (point))
                   (end (goto-char forward-point)))
               (replique-context/object-delimited
                :delimited replique-context/forward-context
                :start start
                :end end)))
            ((eq :string replique-context/forward-context)
             (let ((start (point))
                   (end (goto-char forward-point)))
               (replique-context/object-string
                :string (buffer-substring-no-properties (+ start 1) (- end 1))
                :start start
                :end end)))
            (t
             (let ((start (point))
                   (end (goto-char forward-point)))
               (replique-context/object-symbol
                :symbol (when (> end start)
                          (buffer-substring-no-properties start end))
                :start start
                :end end)))))))

(defun replique-context/meta-value (with-meta)
  (if (and (cl-typep with-meta 'replique-context/object-dispatch-macro)
           (eq :meta (oref with-meta :dispatch-macro)))
      (let ((maybe-value (oref with-meta :value)))
        (while (and (cl-typep maybe-value 'replique-context/object-dispatch-macro)
                    (eq :meta (oref maybe-value :dispatch-macro)))
          (setq maybe-value (oref with-meta :value)))
        maybe-value)
    with-meta))

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
  (when (and (not (eq :discard replique-context/dispatch-macro))
             (or (equal replique-context/quoted :unquote)
                 (and (null replique-context/quoted)
                      (null replique-context/global-quoted))))
    (replique-context/forward-comment)
    (let ((maybe-in-ns (replique-context/meta-value (replique-context/read-one))))
      (when (and maybe-in-ns (cl-typep maybe-in-ns 'replique-context/object-symbol))
        (when (seq-find (lambda (sym)
                          (equal (oref maybe-in-ns :symbol) (symbol-name sym)))
                        replique-context/in-ns-forms)
          (replique-context/forward-comment)
          (let* ((maybe-ns (replique-context/meta-value (replique-context/read-one)))
                 (maybe-ns (if (cl-typep maybe-ns 'replique-context/object-quoted)
                               (replique-context/meta-value (oref maybe-ns :value))
                             maybe-ns)))
            (when (cl-typep maybe-ns 'replique-context/object-symbol)
              (oref maybe-ns :symbol))))))))

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
          (progn
            (cond ((replique-context/delimited-forward-context?)
                   (let ((before-target-point? (< (1- forward-point) target-point)))
                     (if before-target-point?
                         (progn
                           (when (eq :list replique-context/forward-context)
                             (forward-char)
                             (let ((namespace (replique-context/maybe-at-in-ns
                                               (1- forward-point))))
                               (when namespace
                                 (setq replique-context/namespace namespace))))
                           (goto-char forward-point))
                       (replique-context/update-global-quoted-state)
                       (forward-char)))
                   (setq replique-context/dispatch-macro nil)
                   (setq replique-context/quoted nil))
                  ((eq :string replique-context/forward-context)
                   (goto-char forward-point)
                   (setq replique-context/dispatch-macro nil)
                   (setq replique-context/quoted nil))
                  ((eq :dispatch-macro replique-context/forward-context)
                   (goto-char forward-point))
                  ((eq :quoted replique-context/forward-context)
                   ;; clear the dispatch-macro
                   (setq replique-context/dispatch-macro nil)
                   (goto-char forward-point))
                  ((eq :symbol replique-context/forward-context)
                   (setq replique-context/dispatch-macro nil)
                   (setq replique-context/quoted nil)
                   (goto-char forward-point)))
            (replique-context/forward-comment))
        (goto-char target-point))))
  replique-context/namespace)

;; fn context
;; binding context
(defvar replique-context/wrapper-context nil)
(defvar replique-context/wrapper-context-position nil)
(defvar replique-context/fn-context nil)
(defvar replique-context/binding-context nil)

(defun replique-context/collect-locals (target-point)
  (when (and (or (null replique-context/dispatch-macro)
                 (eq replique-context/dispatch-macro :tagged-literal))
             (or (equal replique-context/quoted :unquote)
                 (and (null replique-context/quoted)
                      (null replique-context/global-quoted))))
    ))

(defun replique-context/scan-backward-symbol ()
  (let ((backward-p (skip-chars-backward "^[\s,\(\)\[\]\{\}\"\n\t]*")))
    (goto-char p)))

;; fn-like let-like for-like letfn-like

;; collect-locals-includes-points

(defun replique-context/walk (target-point)
  (while (< (point) target-point)
    (let* ((forward-point (replique-context/scan-forward)))
      (if forward-point
          (cond ((replique-context/delimited-forward-context?)
                 (let ((before-target-point? (< (1- forward-point) target-point)))
                   (if before-target-point?
                       (progn
                         (when (and (eq :vector replique-context/forward-context)
                                    (or (eq :fn-like replique-context/binding-context)
                                        (eq :let-like replique-context/binding-context)
                                        (eq :for-like replique-context/binding-context)
                                        ;; (eq :letfn-like replique-context/binding-context)
                                        ))
                           (replique-context/collect-locals (1- forward-point))
                           (setq replique-context/binding-context nil))
                         (setq replique-context/wrapper-context-position
                               (+ 1 replique-context/wrapper-context-position))
                         (goto-char forward-point))
                     (replique-context/update-global-quoted-state)
                     (setq replique-context/wrapper-context replique-context/forward-context)
                     (cond ((and (eq :vector replique-context/forward-context)
                                 (or (eq :fn-like replique-context/binding-context)
                                     (eq :let-like replique-context/binding-context)
                                     (eq :for-like replique-context/binding-context)
                                     ;; (eq :letfn-like replique-context/binding-context)
                                     ))
                            (replique-context/collect-locals-includes-point target-point))
                           ((eq :list replique-context/forward-context)
                            (if (eq :fn-like replique-context/binding-context)
                                (setq replique-context/binding-context :let-like)
                              (setq replique-context/binding-context nil)))
                           (t (setq replique-context/binding-context nil))))))
                ((eq :symbol replique-context/forward-context)
                 (cond ((and (eq replique-context/wrapper-context :list)
                             (eq replique-context/wrapper-context-position 0)
                             ;; check quoted, dispatch-macro
                             )
                        ;;(setq replique-context/fn-context ...)
                        ))))
        (goto-char target-point)))))

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

(comment
 (defvar replique/extracted-bindings (replique/hash-map))
 (defvar replique/extract-bindings-done nil)

 (defun replique-context/extract-bindings-vector (target-point bindings)
   (while (< (point) target-point)
     (let ((object (replique-context/meta-value (replique-context/read-one))))
       (when (not (and (cl-typep object 'replique-context/object-symbol)
                       (eq "&" (oref object :symbol))))
         (replique-context/extract-bindings object bindings)))
     (replique-context/forward-comment)))

 (defun replique-context/namspaced-keyword-name (object))
 
 (defun replique-context/extract-bindings-map (target-point bindings)
   (while (< (point) target-point)
     (let* ((object-k (replique-context/read-one))
            (object-k-without-meta (replique-context/meta-value object-k))
            (object-v (replique-context/read-one))
            (object-v-without-meta (replique-context/meta-value (replique-context/read-one)))
            (namespaced-keyword-name (replique-context/namespaced-keyword-name
                                      object-k-without-meta)))
       (cond ((and (cl-typep object-k-without-meta 'replique-context/object-symbol)
                   (or (equal ":keys" (oref object-k-without-meta :symbol))
                       (equal ":strs" (oref object-k-without-meta :symbol))
                       (equal ":syms" (oref object-k-without-meta :symbol)))
                   (cl-typep object-v-without-meta 'replique-context/object-delimited)
                   (eq :vector (oref object-v-without-meta :delimited)))
              nil)
             (namespaced-keyword-name nil)
             (t (replique-context/extract-bindings object-k bindings))))
     (replique-context/forward-comment)))
 
 (defun replique-context/extract-bindings (object bindings)
   (cond ((and (cl-typep object 'replique-context/object-delimited)
               (eq :vector (oref object :delimited)))
          (goto-char (+ 1 (oref object :start)))
          (replique-context/forward-comment)
          (replique-context/extract-bindings-vector (- (oref object :end) 1) bindings))
         ((and (cl-typep object 'replique-context/object-delimited)
               (eq :map (oref object :delimited)))
          (goto-char (+ 1 (oref object :start)))
          (replique-context/forward-comment)
          (replique-context/extract-bindings-map (- (oref object :end) 1) bindings))
         ((cl-typep object 'replique-context/object-symbol)
          (let ((sym (oref object :symbol)))
            (when sym
              (puthash sym [(oref object :start) (oref object :end) nil] bindings))))
         ((and (cl-typep object 'replique-context/object-dispatch-macro)
               (eq :meta (oref object :dispatch-macro)))
          (let ((meta-value (replique-context/meta-value object)))
            (cond ((cl-typep meta-value 'replique-context/object-symbol)
                   (let ((sym (oref meta-value :symbol)))
                     (when sym
                       (puthash sym [(oref meta-value :start) (oref meta-value :end)
                                     (oref object :data)]
                                bindings))))
                  ((cl-typep meta-value 'replique-context/object-delimited)
                   )))))
   bindings)
 )

(provide 'replique-context)


