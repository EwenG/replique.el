;; replique-context.el ---   -*- lexical-binding: t; -*-

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

(require 'replique-hashmap)

(defvar replique-context/context nil)

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
  (setq replique-context/namespace nil)

  (setq replique-context/wrapper-context nil)
  (setq replique-context/wrapper-context-position nil)
  (setq replique-context/fn-context nil)
  (setq replique-context/named-fn-context nil)
  (setq replique-context/fn-context-position nil)
  (setq replique-context/param nil)
  (setq replique-context/param-meta nil)
  (setq replique-context/binding-context nil)
  (setq replique-context/locals (replique/hash-map))
  (setq replique-context/in-string? nil)
  (setq replique-context/dependency-context nil)
  (setq replique-context/in-ns-form? nil)
  (setq replique-context/at-binding-position? nil))

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
                  (skip-chars-forward "^[\s,\(\)\[\]\{\}\"\n\t]")
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
                     (skip-chars-forward "^[\s,\(\)\[\]\{\}\"\n\t]")
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
   (data-start :initarg :data-start)
   (data-end :initarg :data-end)
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
                    (replique-context/read-one))
                   ((or (eq :meta replique-context/dispatch-macro)
                        (eq :tagged-literal replique-context/dispatch-macro))
                    (goto-char forward-point)
                    (replique-context/forward-comment)
                    (let ((dispatch-macro replique-context/dispatch-macro)
                          (data-start (point))
                          (data (replique-context/read-one))
                          (data-end (point))
                          (_ (replique-context/forward-comment))
                          (value (replique-context/read-one)))
                      (replique-context/object-dispatch-macro
                       :dispatch-macro dispatch-macro
                       :data data
                       :data-start data-start
                       :data-end data-end
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
               (when (> end start)
                 (replique-context/object-symbol
                  :symbol (buffer-substring-no-properties start end)
                  :start start
                  :end end))))))))

(defun replique-context/meta-value (with-meta)
  (if (and (cl-typep with-meta 'replique-context/object-dispatch-macro)
           (eq :meta (oref with-meta :dispatch-macro)))
      (let ((maybe-value (oref with-meta :value)))
        (while (and (cl-typep maybe-value 'replique-context/object-dispatch-macro)
                    (eq :meta (oref maybe-value :dispatch-macro)))
          (setq maybe-value (oref with-meta :value)))
        maybe-value)
    with-meta))

(defun replique-context/extracted-value (object)
  (if (or (and (cl-typep object 'replique-context/object-dispatch-macro)
               (eq :meta (oref object :dispatch-macro)))
          (and (cl-typep object 'replique-context/object-quoted)
               (or (eq :quote (oref object :quoted))
                   (eq :unquote (oref object :quoted)))))
      (let ((maybe-value (oref object :value)))
        (while (or (and (cl-typep maybe-value 'replique-context/object-dispatch-macro)
                        (eq :meta (oref maybe-value :dispatch-macro)))
                   (and (cl-typep maybe-value 'replique-context/object-quoted)
                        (or (eq :quote (oref maybe-value :quoted))
                            (eq :unquote (oref maybe-value :quoted)))))
          (setq maybe-value (oref object :value)))
        maybe-value)
    object))

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

(defun replique-context/convert-namespaced-keyword (object)
  (if (cl-typep object 'replique-context/object-symbol)
      (let ((name (save-match-data
                    (when (string-match "^\\(:\\|::\\)[^:/]+/\\([^/]+\\)$" (oref object :symbol))
                      (match-string-no-properties 2 (oref object :symbol))))))
        (if name
            (oset object :symbol name)
          object))
    object))

(comment
 (let ((s ":e/ff"))
   (when (string-match "^\\(:\\|::\\)[^:/]+/\\([^/]+\\)$" s)
     (match-string-no-properties 2 s)))
 )

(defun replique-context/add-local-binding (sym sym-start sym-end meta)
  (puthash sym `[,sym-start ,sym-end ,meta] replique-context/locals))

(defun replique-context/extract-bindings-vector (target-point
                                                 binding-found-fn
                                                 &optional namespaced-keys)
  (while (< (point) target-point)
    (let* ((object (replique-context/meta-value (replique-context/read-one)))
           (object (if namespaced-keys
                       (replique-context/convert-namespaced-keyword object)
                     object)))
      (if object
          (progn
            (when (not (and (cl-typep object 'replique-context/object-symbol)
                            (equal "&" (oref object :symbol))))
              (replique-context/extract-bindings object binding-found-fn))
            (replique-context/forward-comment))
        (goto-char (+ 1 target-point)))))
  (goto-char (+ 1 target-point)))

(defun replique-context/extract-bindings-map (target-point binding-found-fn)
  (while (< (point) target-point)
    (let* ((object-k (replique-context/read-one))
           (object-k-meta-value (replique-context/meta-value object-k))
           (object-k-meta-value (replique-context/convert-namespaced-keyword
                                   object-k-meta-value))
           (_ (replique-context/forward-comment))
           (object-v (replique-context/read-one))
           (object-v-extracted (replique-context/extracted-value object-v))
           (object-v-meta-value (replique-context/meta-value object-v)))
      (if (and object-k object-v)
          (let ((p (point)))
            (cond ((and (cl-typep object-k-meta-value 'replique-context/object-symbol)
                        (or (equal ":keys" (oref object-k-meta-value :symbol))
                            (equal ":strs" (oref object-k-meta-value :symbol))
                            (equal ":syms" (oref object-k-meta-value :symbol)))
                        (cl-typep object-v-extracted 'replique-context/object-delimited)
                        (eq :vector (oref object-v-extracted :delimited)))
                   (goto-char (+ 1 (oref object-v-extracted :start)))
                   (replique-context/extract-bindings-vector
                    (- (oref object-v-extracted :end) 1) binding-found-fn t))
                  ((and (cl-typep object-k-meta-value 'replique-context/object-symbol)
                        (equal ":or" (oref object-k-meta-value :symbol))
                        (cl-typep object-v-extracted 'replique-context/object-delimited)
                        (eq :map (oref object-v-extracted :delimited)))
                   (goto-char (+ 1 (oref object-v-extracted :start)))
                   (replique-context/extract-bindings-map
                    (- (oref object-v-extracted :end) 1) binding-found-fn))
                  ((and (cl-typep object-k-meta-value 'replique-context/object-symbol)
                        (equal ":as" (oref object-k-meta-value :symbol))
                        (cl-typep object-v-meta-value 'replique-context/object-symbol))
                   (replique-context/extract-bindings object-v binding-found-fn))
                  (t (replique-context/extract-bindings object-k binding-found-fn)))
            (goto-char p)
            (replique-context/forward-comment))
        (goto-char (+ 1 target-point)))))
  (goto-char (+ target-point 1)))

(defun replique-context/extract-bindings (object binding-found-fn)
  (let ((object-extracted (replique-context/extracted-value object))
        (object-meta-value (replique-context/meta-value object)))
    (cond ((and (cl-typep object-extracted 'replique-context/object-delimited)
                (eq :vector (oref object-extracted :delimited)))
           (goto-char (+ 1 (oref object-extracted :start)))
           (replique-context/forward-comment)
           (replique-context/extract-bindings-vector (- (oref object :end) 1) binding-found-fn))
          ((and (cl-typep object-extracted 'replique-context/object-delimited)
                (eq :map (oref object-extracted :delimited)))
           (goto-char (+ 1 (oref object-extracted :start)))
           (replique-context/forward-comment)
           (replique-context/extract-bindings-map
            (- (oref object-extracted :end) 1) binding-found-fn))
          ((cl-typep object-meta-value 'replique-context/object-symbol)
           (let ((sym (oref object-meta-value :symbol))
                 (meta (when (and (cl-typep object 'replique-context/object-dispatch-macro)
                                  (eq :meta (oref object :dispatch-macro)))
                         (buffer-substring-no-properties (oref object :data-start)
                                                         (oref object :data-end)))))
             (when (and sym (not (string-prefix-p ":" sym)))
               (funcall binding-found-fn sym
                        (oref object-meta-value :start)
                        (oref object-meta-value :end)
                        meta)))))))

(defun replique-context/extract-named-fn (object object-meta-value)
  (let ((sym (oref object-meta-value :symbol))
        (meta (when (and (cl-typep object 'replique-context/object-dispatch-macro)
                         (eq :meta (oref object :dispatch-macro)))
                (buffer-substring-no-properties (oref object :data-start)
                                                (oref object :data-end)))))
    (when (and sym (not (string-prefix-p ":" sym)))
      (setq replique-context/named-fn-context `[,sym [,(oref object-meta-value :start)
                                                      ,(oref object-meta-value :end)
                                                      ,meta]]))))

(defun replique-context/extract-params-bindings (target-point binding-found-fn)
  (when replique-context/named-fn-context
    (puthash (aref replique-context/named-fn-context 0)
             (aref replique-context/named-fn-context 1)
             replique-context/locals))
  (while (< (point) target-point)
    (let* ((object (replique-context/read-one))
           (p (point)))
      (if object
          (progn
            (replique-context/extract-bindings object binding-found-fn)
            (goto-char p)
            (replique-context/forward-comment))
        (goto-char target-point)))))

(defvar replique-context/wrapper-context nil)
(defvar replique-context/wrapper-context-position nil)
(defvar replique-context/fn-context nil)
(defvar replique-context/named-fn-context nil)
(defvar replique-context/fn-context-position nil)
(defvar replique-context/param nil)
(defvar replique-context/param-meta nil)
(defvar replique-context/binding-context nil)
(defvar replique-context/locals nil)
(defvar replique-context/in-string? nil)
(defvar replique-context/dependency-context nil)
(defvar replique-context/in-ns-form? nil)
(defvar replique-context/at-binding-position? nil)

(defun replique-context/object-leaf (object)
  (when object
    (while (or (cl-typep object 'replique-context/object-dispatch-macro)
               (cl-typep object 'replique-context/object-quoted))
      (setq object (oref object :value)))
    object))

(defun replique-context/at-for-like-let? (object-k-meta-value object-v-meta-value)
  (and (eq :for-like replique-context/binding-context)
       (cl-typep object-k-meta-value 'replique-context/object-symbol)
       (equal ":let" (oref object-k-meta-value :symbol))
       (cl-typep object-v-meta-value 'replique-context/object-delimited)
       (eq :vector (oref object-v-meta-value :delimited))))

(defun replique-context/collect-locals1 (target-point)
  (let ((exit nil))
    (while (and (< (point) target-point) (null exit))
      (let* ((start-p (point))
             (object-k (replique-context/read-one))
             (object-k-meta-value (replique-context/meta-value object-k))
             (_ (replique-context/forward-comment))
             (object-v (replique-context/read-one))
             (object-v-meta-value (replique-context/meta-value object-v))
             (object-v-leaf (replique-context/object-leaf object-v)))
        (if (and object-k object-v-meta-value)
            (cond ((replique-context/at-for-like-let? object-k-meta-value object-v-meta-value)
                   (cond ((<= target-point (oref object-v-meta-value :start))
                          (goto-char start-p)
                          (setq exit t))
                         ((>= target-point (oref object-v-meta-value :end))
                          (let ((binding-context replique-context/binding-context))
                            (goto-char (+ 1 (oref object-v-meta-value :start)))
                            (replique-context/forward-comment)
                            (setq replique-context/binding-context :let-like)
                            (replique-context/collect-locals2
                             (- (oref object-v-meta-value :end) 1))
                            (setq replique-context/wrapper-context-position
                                  (+ 2 replique-context/wrapper-context-position))
                            (goto-char (oref object-v-meta-value :end))
                            (setq replique-context/binding-context binding-context)
                            (replique-context/forward-comment)))
                         (t
                          (setq replique-context/wrapper-context-position 0)
                          (setq replique-context/binding-context :let-like)
                          (goto-char (+ 1 (oref object-v-meta-value :start)))
                          (replique-context/forward-comment)
                          (replique-context/collect-locals1 target-point)
                          (setq exit t))))
                  ((< (oref object-v-leaf :end) target-point)
                   (replique-context/extract-bindings
                    object-k 'replique-context/add-local-binding)
                   (setq replique-context/wrapper-context-position
                         (+ 2 replique-context/wrapper-context-position))
                   (goto-char (oref object-v-leaf :end))
                   (replique-context/forward-comment))
                  (t
                   (replique-context/extract-bindings
                    object-k (lambda (sym sym-start sym-end sym-meta)
                               (when (<= sym-start target-point sym-end)
                                 (setq replique-context/at-binding-position? t))))
                   (goto-char start-p)
                   (setq exit t)))
          (setq replique-context/at-binding-position? t)
          (goto-char target-point))))))

(defun replique-context/collect-locals2 (target-point)
  (while (< (point) target-point)
    (let* ((object-k (replique-context/read-one))
           (object-k-meta-value (replique-context/meta-value object-k))
           (_ (replique-context/forward-comment))
           (object-v-meta-value (replique-context/meta-value (replique-context/read-one)))
           (p (point)))
      (if (and object-k object-v-meta-value)
          (if (replique-context/at-for-like-let? object-k-meta-value object-v-meta-value)
              (let ((binding-context replique-context/binding-context))
                (goto-char (+ 1 (oref object-v-meta-value :start)))
                (replique-context/forward-comment)
                (setq replique-context/binding-context :let-like)
                (replique-context/collect-locals2 (- (oref object-v-meta-value :end) 1))
                (goto-char (oref object-v-meta-value :end))
                (setq replique-context/binding-context binding-context)
                (replique-context/forward-comment))
            (replique-context/extract-bindings object-k 'replique-context/add-local-binding)
            (goto-char p)
            (replique-context/forward-comment))
        (goto-char target-point)))))

(defvar replique-context/target-point nil)
(defvar replique-context/last-read nil)

(defun replique-context/pushback-read-one ()
  (let ((p (point))
        (object-leaf (replique-context/object-leaf (replique-context/read-one))))
    (goto-char p)
    object-leaf))

(defun replique-context/skip-until-target-point (&optional other-stop-condition-fn)
  (let* ((stop-recur nil)
         (object nil)
         (object-leaf nil))
    (while (null stop-recur)
      (let ((p (point)))
        (setq object (replique-context/read-one))
        (setq object-leaf (replique-context/object-leaf object))
        (if (or (null object-leaf)
                (<= replique-context/target-point (oref object-leaf :end))
                (when other-stop-condition-fn
                  (funcall other-stop-condition-fn object object-leaf)))
            (progn (setq stop-recur t)
                   (goto-char p))
          (replique-context/forward-comment))))))

(defun replique-context/keyword-then-target-point (object object-leaf)
  (let ((object-meta-value (replique-context/meta-value object)))
    (when (and (cl-typep object-meta-value 'replique-context/object-symbol)
               (string-prefix-p ":" (oref object-meta-value :symbol)))
      (replique-context/forward-comment)
      (let ((next-object-leaf (replique-context/pushback-read-one)))
        (when next-object-leaf
          (<= replique-context/target-point (oref next-object-leaf :end)))))))

(defmacro replique-context/restore-point-on-failure (&rest body)
  (let ((point-sym (make-symbol "p"))
        (success-sym (make-symbol "success?")))
    `(let ((,point-sym (point)))
       (let ((,success-sym (progn ,@body)))
         (when (not ,success-sym)
           (goto-char ,point-sym))
         ,success-sym))))

(defun replique-context/in-sequential ()
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'replique-context/object-delimited)
                       (or (eq :list (oref object-extracted :delimited))
                           (eq :vector (oref object-extracted :delimited)))
                       (> replique-context/target-point (oref object-extracted :start))
                       (< replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted)
       (goto-char (+ (oref object-extracted :start) 1))
       (replique-context/forward-comment))
     result)))

(defun replique-context/after-sequential ()
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'replique-context/object-delimited)
                       (or (eq :list (oref object-extracted :delimited))
                           (eq :vector (oref object-extracted :delimited)))
                       (>= replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted)
       (replique-context/forward-comment))
     result)))

(defun replique-context/in-keyword ()
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-meta-value (replique-context/meta-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-meta-value 'replique-context/object-symbol)
                       (string-prefix-p ":" (oref object-meta-value :symbol))
                       (>= replique-context/target-point (oref object-meta-value :start))
                       (<= replique-context/target-point (oref object-meta-value :end)))))
     (when result
       (setq replique-context/last-read object-meta-value))
     result)))

(defun replique-context/after-keyword (&optional keywords-filter)
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-meta-value (replique-context/meta-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-meta-value 'replique-context/object-symbol)
                       (string-prefix-p ":" (oref object-meta-value :symbol))
                       (cond ((null keywords-filter) t)
                             ((listp keywords-filter) (seq-find (lambda (k)
                                                                  (equal
                                                                   (oref object-meta-value :symbol)
                                                                   k))
                                                                keywords-filter))
                             (t (equal (oref object-meta-value :symbol) keywords-filter)))
                       (> replique-context/target-point (oref object-meta-value :end)))))
     (when result
       (setq replique-context/last-read object-meta-value)
       (replique-context/forward-comment))
     result)))

(defun replique-context/in-symbol ()
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'replique-context/object-symbol)
                       (not (string-prefix-p ":" (oref object-extracted :symbol)))
                       (>= replique-context/target-point (oref object-extracted :start))
                       (<= replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted))
     result)))

(defun replique-context/after-symbol ()
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'replique-context/object-symbol)
                       (not (string-prefix-p ":" (oref object-extracted :symbol)))
                       (> replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted)
       (replique-context/forward-comment))
     result)))

(defun replique-context/in-string ()
  (replique-context/restore-point-on-failure
   (let* ((object (replique-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'replique-context/object-string)
                       (> replique-context/target-point (oref object-extracted :start))
                       (< replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted))
     result)))

(defun replique-context/libspec (position-libspec-option position-namespace prefix)
  (cond ((replique-context/in-symbol)
         (setq replique-context/dependency-context (replique/hash-map
                                                    :position position-namespace
                                                    :prefix prefix))
         t)
        ((replique-context/after-symbol)
         (let* ((object-namespace replique-context/last-read)
                (prefix (if (equal "" prefix)
                            (oref object-namespace :symbol)
                          (concat prefix "." (oref object-namespace :symbol)))))
           (replique-context/skip-until-target-point
            'replique-context/keyword-then-target-point)
           (cond ((replique-context/in-sequential)
                  (replique-context/libspec position-libspec-option position-namespace prefix))
                 ((replique-context/in-symbol)
                  (setq replique-context/dependency-context (replique/hash-map
                                                             :position position-namespace
                                                             :prefix prefix))
                  t)
                 ((replique-context/in-keyword)
                  (setq replique-context/dependency-context
                        (replique/hash-map :position position-libspec-option))
                  t)
                 ((replique-context/after-keyword)
                  (let ((object-keyword replique-context/last-read))
                    (when (and (equal ":refer" (oref object-keyword :symbol))
                               (replique-context/in-sequential))
                      (replique-context/skip-until-target-point)
                      (when (replique-context/in-symbol)
                        (setq replique-context/dependency-context
                              (replique/hash-map :position :var :namespace prefix))
                        t)))))))))

(defun replique-context/libspec-refer (namespace)
  (cond ((replique-context/in-keyword)
         (setq replique-context/dependency-context
               (replique/hash-map :position :libspec-option-refer))
         t)
        ((replique-context/after-keyword)
         (let ((object-keyword replique-context/last-read))
           (when (and (or (equal ":only" (oref object-keyword :symbol))
                          (equal ":exclude" (oref object-keyword :symbol)))
                      (replique-context/in-sequential))
             (replique-context/skip-until-target-point)
             (when (replique-context/in-symbol)
               (setq replique-context/dependency-context
                     (replique/hash-map :position :var :namespace namespace))
               t))))))

(defun replique-context/import-spec ()
  (cond ((replique-context/in-symbol)
         (setq replique-context/dependency-context
               (replique/hash-map :position :package-or-class))
         t)
        ((replique-context/in-sequential)
         (cond ((replique-context/in-symbol)
                (setq replique-context/dependency-context
                      (replique/hash-map :position :package-or-class))
                t)
               ((replique-context/after-symbol)
                (let ((object-package replique-context/last-read))
                  (replique-context/skip-until-target-point)
                  (when (replique-context/in-symbol)
                    (setq replique-context/dependency-context
                          (replique/hash-map :position :class
                                             :package (oref object-package :symbol)))
                    t)))))))

(defun replique-context/walk-ns (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (replique-context/skip-until-target-point)
     (when (replique-context/in-sequential)
       (cond ((replique-context/in-keyword)
              (setq replique-context/dependency-context
                    (replique/hash-map :position :dependency-type))
              t)
             ((replique-context/after-keyword '(":require" ":require-macros" ":use"))
              (let* ((object-keyword replique-context/last-read)
                     (position-namespace (if (equal
                                              ":require-macros"
                                              (oref object-keyword :symbol))
                                             :namespace-macros
                                           :namespace))
                     (position-libspec-option (if (equal ":use" (oref object-keyword :symbol))
                                                  :libspec-option-refer
                                                :libspec-option)))
                (replique-context/skip-until-target-point)
                (cond ((replique-context/in-symbol)
                       (setq replique-context/dependency-context
                             (replique/hash-map :position position-namespace :prefix ""))
                       t)
                      ((replique-context/in-sequential)
                       (replique-context/libspec position-libspec-option position-namespace ""))
                      ((replique-context/in-keyword)
                       (setq replique-context/dependency-context
                             (replique/hash-map :position :flag))
                       t))))
             ((replique-context/after-keyword ":import")
              (replique-context/skip-until-target-point)
              (replique-context/import-spec))
             ((replique-context/after-keyword ":refer-clojure")
              (replique-context/libspec-refer :refer-clojure))
             ((replique-context/after-keyword ":load")
              (replique-context/skip-until-target-point)
              (when (replique-context/in-string)
                (setq replique-context/dependency-context
                      (replique/hash-map :position :load-path))
                t)))))))

(defun replique-context/walk-require (target-point use? require-macros?)
  (let ((replique-context/target-point target-point)
        (position-libspec-option (if use? :libspec-option-refer :libspec-option))
        (position-namespace (if require-macros? :namespace-macros :namespace)))
    (replique-context/restore-point-on-failure
     (cond ((replique-context/in-symbol)
            (setq replique-context/dependency-context
                  (replique/hash-map :position position-namespace
                                     :prefix ""))
            t)
           ((replique-context/in-sequential)
            (replique-context/libspec position-libspec-option position-namespace ""))
           ((or (replique-context/after-sequential) (replique-context/after-symbol))
            (when (replique-context/in-keyword)
              (setq replique-context/dependency-context (replique/hash-map :position :flag))
              t))))))

(defun replique-context/walk-import (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (replique-context/import-spec))))

(defun replique-context/walk-refer-clojure (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (replique-context/libspec-refer :refer-clojure))))

(defun replique-context/walk-load (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (replique-context/skip-until-target-point)
     (when (replique-context/in-string)
       (setq replique-context/dependency-context (replique/hash-map :position :load-path))
       t))))

(defun replique-context/walk-refer (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (cond ((replique-context/in-symbol)
            (setq replique-context/dependency-context (replique/hash-map :position :namespace))
            t)
           ((replique-context/after-symbol)
            (let* ((namespace-object replique-context/last-read)
                   (namespace (oref namespace-object :symbol)))
              (replique-context/libspec-refer namespace)))))))

(comment
 (let* ((forward-point (replique-context/walk-init)))
   (goto-char (+ 1 (point)))
   (replique-context/forward-comment)
   (replique-context/walk-ns forward-point)
   (replique-edn/pr-str replique-context/dependency-context))
 )

(defun replique-context/at-binding-context? (object)
  (and (cl-typep object 'replique-context/object-delimited)
       (eq :vector (oref object :delimited))
       (eq 1 replique-context/wrapper-context-position)
       (or (eq :let-like replique-context/binding-context)
           (eq :for-like replique-context/binding-context))))

(defun replique-context/at-named-fn? (target-point object)
  (and (cl-typep object 'replique-context/object-symbol)
       (eq :fn-like replique-context/binding-context)
       (eq 1 replique-context/fn-context-position)))

(defun replique-context/at-fn-params? (object)
  (and (cl-typep object 'replique-context/object-delimited)
       (eq :vector (oref object :delimited))
       (or (and (eq 0 replique-context/wrapper-context-position)
                (eq :method-like replique-context/binding-context))
           (and (eq :fn-like replique-context/binding-context)
                (or (eq 1 replique-context/fn-context-position)
                    (eq 2 replique-context/fn-context-position)
                    (eq 3 replique-context/fn-context-position)
                    (eq 4 replique-context/fn-context-position))))))

(defun replique-context/inc-positions (target-point object)
  (when (> target-point (oref object :end))
    (when replique-context/wrapper-context
      (setq replique-context/wrapper-context-position
            (+ 1 replique-context/wrapper-context-position))
      (when (eq :fn replique-context/wrapper-context)
        (setq replique-context/fn-context-position
              (+ 1 replique-context/fn-context-position))))))

;; fn-like let-like for-like method-like letfn-like

;; Errors (read-one returning nil) may prevent the computation of some contextual values
;; such as the fn-context. In such a case, we reset the contextual states and go
;; to the target-point
(defun replique-context/walk (target-point context-forms)
  (while (< (point) target-point)
    (let* ((object (replique-context/read-one))
           (object-extracted (replique-context/extracted-value object))
           (object-meta-value (replique-context/meta-value object))
           (object-leaf (replique-context/object-leaf object)))
      (if object-leaf
          (progn
            (cond ((and (replique-context/at-binding-context? object-extracted)
                        (> target-point (oref object-extracted :start))
                        (< target-point (oref object-extracted :end)))
                   (setq replique-context/wrapper-context :vector)
                   (setq replique-context/wrapper-context-position 0)
                   (goto-char (+ 1 (oref object-extracted :start)))
                   (replique-context/forward-comment)
                   (replique-context/collect-locals1 target-point)
                   (setq replique-context/binding-context nil))
                  ((and (replique-context/at-binding-context? object-extracted)
                        (>= target-point (oref object-extracted :end)))
                   (goto-char (+ 1 (oref object-extracted :start)))
                   (replique-context/forward-comment)
                   (replique-context/collect-locals2 (- (oref object-extracted :end) 1))
                   (goto-char (oref object-extracted :end))
                   (replique-context/inc-positions target-point object-extracted))
                  ((and (replique-context/at-named-fn? target-point object-meta-value)
                        (>= target-point (oref object-meta-value :end)))
                   (replique-context/extract-named-fn object object-meta-value)
                   (goto-char (oref object-meta-value :end))
                   (replique-context/inc-positions target-point object-meta-value))
                  ((and (replique-context/at-fn-params? object-extracted)
                        (>= target-point (oref object-extracted :end)))
                   (goto-char (+ 1 (oref object-extracted :start)))
                   (replique-context/forward-comment)
                   (replique-context/extract-params-bindings
                    (- (oref object-extracted :end) 1) 'replique-context/add-local-binding)
                   (goto-char (oref object-extracted :end))
                   (replique-context/inc-positions target-point object-extracted))
                  ((and (replique-context/at-fn-params? object-extracted)
                        (< (oref object-extracted :start)
                           target-point
                           (oref object-extracted :end)))
                   (goto-char (+ 1 (oref object-extracted :start)))
                   (replique-context/forward-comment)
                   (replique-context/extract-params-bindings
                    (- (oref object-extracted :end) 1)
                    (lambda (sym sym-start sym-end sym-meta)
                      (when (<= sym-start target-point sym-end)
                        (setq replique-context/at-binding-position? t)))))
                  ((and (cl-typep object-leaf 'replique-context/object-delimited)
                        (eq :list (oref object-leaf :delimited))
                        (> target-point (oref object-leaf :start))
                        (< target-point (oref object-leaf :end)))
                   (setq replique-context/wrapper-context :list)
                   (setq replique-context/wrapper-context-position 0)
                   (if (eq replique-context/binding-context :fn-like)
                       (setq replique-context/binding-context :method-like)
                     (setq replique-context/binding-context nil))
                   (goto-char (+ (oref object-leaf :start) 1)))
                  ((and (cl-typep object-leaf 'replique-context/object-delimited)
                        (> target-point (oref object-leaf :start))
                        (< target-point (oref object-leaf :end)))
                   (setq replique-context/wrapper-context (oref object-leaf :delimited))
                   (setq replique-context/wrapper-context-position 0)
                   (setq replique-context/binding-context nil)
                   (goto-char (+ (oref object-leaf :start) 1)))
                  ((and (cl-typep object-meta-value 'replique-context/object-symbol)
                        (not (string-prefix-p ":" (oref object-meta-value :symbol)))
                        (eq :list replique-context/wrapper-context)
                        (eq 0 replique-context/wrapper-context-position)
                        (>= target-point (oref object-meta-value :start)))
                   (setq replique-context/wrapper-context :fn)
                   (setq replique-context/fn-context (oref object-meta-value :symbol))
                   (setq replique-context/fn-context-position 0)
                   (setq replique-context/named-fn-context nil)
                   (when (null (replique/get replique-context/locals
                                             (oref object-meta-value :symbol)))
                     (let ((binding-context (replique/get
                                             (replique/get context-forms :binding-context)
                                             (oref object-meta-value :symbol)))
                           (dependency-context (replique/get
                                                (replique/get context-forms :dependency-context)
                                                (oref object-meta-value :symbol)))
                           (ns-context (replique/get
                                        (replique/get context-forms :ns-context)
                                        (oref object-meta-value :symbol))))
                       (cond (binding-context
                              (setq replique-context/binding-context binding-context))
                             ((equal ns-context :ns-like)
                              (replique-context/forward-comment)
                              (when (replique-context/walk-ns target-point)
                                (setq replique-context/in-ns-form? t)))
                             ((equal dependency-context :require-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-require target-point nil nil))
                             ((equal dependency-context :use-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-require target-point t nil))
                             ((equal dependency-context :require-macros-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-require target-point nil t))
                             ((equal dependency-context :import-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-import target-point))
                             ((equal dependency-context :refer-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-refer target-point))
                             ((equal dependency-context :refer-clojure-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-refer-clojure target-point))
                             ((equal dependency-context :load-like)
                              (replique-context/forward-comment)
                              (replique-context/walk-load target-point)))))
                   (replique-context/inc-positions target-point object-meta-value))
                  (t (replique-context/inc-positions target-point object-leaf)))
            (replique-context/forward-comment))
        (replique-context/init-state)
        (goto-char target-point))))
  (when (and replique-context/fn-context
             (equal 0 replique-context/fn-context-position))
    (let* ((object (replique-context/read-one))
           (object-meta-value (replique-context/meta-value object)))
      (when (cl-typep object-meta-value 'replique-context/object-symbol)
        (let ((sym (oref object-meta-value :symbol))
              (meta (when (and (cl-typep object 'replique-context/object-dispatch-macro)
                               (eq :meta (oref object :dispatch-macro)))
                      (buffer-substring-no-properties (oref object :data-start)
                                                      (oref object :data-end)))))
          (when sym
            (setq replique-context/param sym)
            (setq replique-context/param-meta meta)))))))

(defun replique-context/walk-init ()
  (let* ((target-point (point))
         (ppss (syntax-ppss target-point))
         (top-level (syntax-ppss-toplevel-pos ppss)))
    (when top-level
      (goto-char top-level)
      (skip-chars-backward "^[\s,\(\)\[\]\{\}\"\n\t]")
      (replique-context/forward-comment)
      (replique-context/init-state)
      (setq replique-context/in-string? (not (null (nth 3 ppss))))
      (setq replique-context/in-comment? (not (null (nth 4 ppss))))
      target-point)))

(comment
 (let* ((forward-point (replique-context/walk-init)))
   (replique-context/walk-in-ns forward-point))

 (let* ((forward-point (replique-context/walk-init)))
   (replique-context/walk forward-point nil)
   replique-context/locals
   replique-context/fn-context
   replique-context/fn-context-position)
 )

;; find in-ns calls in the current form. If no namespace is found, use clojure-find-ns
(defun replique-context/clojure-find-ns ()
  (save-excursion
    (let* ((forward-point (replique-context/walk-init))
           (namespace (when forward-point (replique-context/walk-in-ns forward-point))))
      (or namespace (clojure-find-ns)))))

(defun replique-context/build-context ()
  (replique/hash-map
   :locals replique-context/locals
   :in-string? replique-context/in-string?
   :in-comment? replique-context/in-comment?
   :dependency-context replique-context/dependency-context
   :in-ns-form? replique-context/in-ns-form?
   :param replique-context/param
   :param-meta replique-context/param-meta
   :fn-context replique-context/fn-context
   :fn-context-position replique-context/fn-context-position
   :at-binding-position? replique-context/at-binding-position?))

(defun replique-context/get-context (repl-env)
  (or replique-context/context
      (let* ((tooling-repl (replique/active-repl :tooling))
             (resp (replique/send-tooling-msg
                    tooling-repl
                    (replique/hash-map :type :context
                                       :repl-env repl-env
                                       :ns (replique-context/clojure-find-ns)))))
        (let ((err (replique/get resp :error)))
          (if err
              (progn
                (message "%s" (replique-edn/pr-str err))
                (message "context failed"))
            (save-excursion
              (let ((forward-point (replique-context/walk-init)))
                (when forward-point
                  (replique-context/walk forward-point resp)
                  (setq replique-context/context (replique-context/build-context))
                  replique-context/context))))))))

(defun replique-context/reset-state ()
  (setq replique-context/context nil))

(add-hook 'post-command-hook 'replique-context/reset-state)

(comment
 
 (setq oo (replique-context/read-one))
 (replique-context/extract-bindings oo (lambda (sym sym-start sym-end meta) (print sym)))

 (progn
   (replique-context/get-context 'print)
   (replique-edn/pr-str replique-context/locals))
 )

(provide 'replique-context)

;; letfn-like
;; unwrap comment
;; context for datastructures (narrowing ...)
