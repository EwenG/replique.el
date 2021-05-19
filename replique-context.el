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

(require 'clj-data)
(require 'clj-context)
(require 'clj-pprint)
(require 'replique-repls)

(defvar replique-context/locals nil)
(defvar replique-context/at-binding-position? nil)
(defvar replique-context/at-local-binding-position? nil)
(defvar replique-context/in-ns-form? nil)
(defvar replique-context/dependency-context nil)
(defvar replique-context/fn-context nil)
(defvar replique-context/fn-context-position nil)
(defvar replique-context/fn-param nil)
(defvar replique-context/fn-param-meta nil)

(defun replique-context/reset-state ()
  (setq replique-context/locals (clj-data/hash-map))
  (setq replique-context/at-binding-position? nil)
  (setq replique-context/at-local-binding-position? nil)
  (setq replique-context/in-ns-form? nil)
  (setq replique-context/dependency-context nil)
  (setq replique-context/fn-context nil)
  (setq replique-context/fn-context-position 0)
  (setq replique-context/fn-param nil)
  (setq replique-context/fn-param-meta nil))

(defun replique-context/extracted-value (object)
  (if (or (and (cl-typep object 'clj-context/object-dispatch-macro)
               (eq :meta (oref object :dispatch-macro)))
          (and (cl-typep object 'clj-context/object-quoted)
               (or (eq :quote (oref object :quoted))
                   (eq :unquote (oref object :quoted)))))
      (let ((maybe-value (oref object :value)))
        (while (or (and (cl-typep maybe-value 'clj-context/object-dispatch-macro)
                        (eq :meta (oref maybe-value :dispatch-macro)))
                   (and (cl-typep maybe-value 'clj-context/object-quoted)
                        (or (eq :quote (oref maybe-value :quoted))
                            (eq :unquote (oref maybe-value :quoted)))))
          (setq maybe-value (oref maybe-value :value)))
        maybe-value)
    object))

(defun replique-context/object-leaf (object)
  (when object
    (while (or (cl-typep object 'clj-context/object-dispatch-macro)
               (cl-typep object 'clj-context/object-quoted)
               (cl-typep object 'clj-context/object-deref))
      (setq object (oref object :value)))
    object))

(defun replique-context/add-local-binding (target-point sym sym-start sym-end meta)
  (puthash sym `[,sym-start ,sym-end ,meta] replique-context/locals))

(defun replique-context/maybe-at-local-binding-position (target-point sym sym-start sym-end meta)
  (when (<= sym-start target-point sym-end)
    (setq replique-context/at-local-binding-position? t)))

(defun replique-context/convert-namespaced-keyword (object)
  (when (cl-typep object 'clj-context/object-symbol)
    (when-let (name (save-match-data
                      (when (string-match "^\\(::?\\)\\([^:/]+/\\)?\\([^:/]+\\)$"
                                          (oref object :symbol))
                        (match-string-no-properties 3 (oref object :symbol)))))
      (oset object :symbol name)))
  object)

(comment
 (let ((s ":ee/ff"))
   (when (string-match "^\\(::?\\)\\([^:/]+/\\)?\\([^:/]+\\)$" s)
     (match-string-no-properties 3 s)))
 )

(defun replique-context/extract-bindings-vector (target-point
                                                 binding-found-fn
                                                 &optional namespaced-keys)
  (let ((exit nil))
    (while (null exit)
      (let* ((object (clj-context/meta-value (clj-context/read-one)))
             (object (if namespaced-keys
                         (replique-context/convert-namespaced-keyword object)
                       object)))
        (if object
            (progn
              (when (not (and (cl-typep object 'clj-context/object-symbol)
                              (equal "&" (oref object :symbol))))
                (replique-context/extract-bindings target-point object binding-found-fn))
              (clj-context/forward-comment))
          (setq exit t))))))

(defun replique-context/extract-bindings-map (target-point binding-found-fn)
  (let ((exit nil))
    (while (null exit)
      (let* ((object-k (clj-context/read-one))
             (object-k-meta-value (clj-context/meta-value object-k))
             (_ (clj-context/forward-comment))
             (object-v (clj-context/read-one))
             (object-v-extracted (replique-context/extracted-value object-v))
             (object-v-meta-value (clj-context/meta-value object-v)))
        (if (and object-k object-v)
            (let ((p (point)))
              (cond ((and (cl-typep object-k-meta-value 'clj-context/object-symbol)
                          (or (equal ":keys" (oref object-k-meta-value :symbol))
                              (equal ":strs" (oref object-k-meta-value :symbol))
                              (equal ":syms" (oref object-k-meta-value :symbol)))
                          (cl-typep object-v-extracted 'clj-context/object-delimited)
                          (eq :vector (oref object-v-extracted :delimited)))
                     (goto-char (+ 1 (oref object-v-extracted :start)))
                     (replique-context/extract-bindings-vector target-point binding-found-fn t)
                     (goto-char (oref object-v-extracted :end)))
                    ((and (cl-typep object-k-meta-value 'clj-context/object-symbol)
                          (equal ":or" (oref object-k-meta-value :symbol))
                          (cl-typep object-v-extracted 'clj-context/object-delimited)
                          (eq :map (oref object-v-extracted :delimited)))
                     (goto-char (+ 1 (oref object-v-extracted :start)))
                     (replique-context/extract-bindings-map target-point binding-found-fn)
                     (goto-char (oref object-v-extracted :end)))
                    ((and (cl-typep object-k-meta-value 'clj-context/object-symbol)
                          (equal ":as" (oref object-k-meta-value :symbol))
                          (cl-typep object-v-meta-value 'clj-context/object-symbol))
                     (replique-context/extract-bindings target-point object-v binding-found-fn))
                    (t (replique-context/extract-bindings target-point object-k binding-found-fn)))
              (goto-char p)
              (clj-context/forward-comment))
          (setq exit t))))))

(defun replique-context/extract-bindings (target-point object binding-found-fn)
  (let ((object-extracted (replique-context/extracted-value object))
        (object-meta-value (clj-context/meta-value object)))
    (cond ((and (cl-typep object-extracted 'clj-context/object-delimited)
                (eq :vector (oref object-extracted :delimited)))
           (goto-char (+ 1 (oref object-extracted :start)))
           (clj-context/forward-comment)
           (replique-context/extract-bindings-vector target-point binding-found-fn)
           (goto-char (oref object :end)))
          ((and (cl-typep object-extracted 'clj-context/object-delimited)
                (eq :map (oref object-extracted :delimited)))
           (goto-char (+ 1 (oref object-extracted :start)))
           (clj-context/forward-comment)
           (replique-context/extract-bindings-map target-point binding-found-fn)
           (goto-char (oref object-extracted :end)))
          ((cl-typep object-meta-value 'clj-context/object-symbol)
           (let ((sym (oref object-meta-value :symbol))
                 (meta (when (and (cl-typep object 'clj-context/object-dispatch-macro)
                                  (eq :meta (oref object :dispatch-macro)))
                         (buffer-substring-no-properties (oref object :data-start)
                                                         (oref object :data-end)))))
             (when (and sym (not (string-prefix-p ":" sym)))
               (funcall binding-found-fn target-point sym
                        (oref object-meta-value :start)
                        (oref object-meta-value :end)
                        meta)))))))

(defun replique-context/at-for-like-let? (for-like? object-k-meta-value object-v-meta-value) ;
  (and for-like?
       (cl-typep object-k-meta-value 'clj-context/object-symbol)
       (equal ":let" (oref object-k-meta-value :symbol))
       (cl-typep object-v-meta-value 'clj-context/object-delimited)
       (eq :vector (oref object-v-meta-value :delimited))))

(defun replique-context/handle-binding-vector (target-point &optional for-like?)
  (let ((exit nil))
    (while (null exit)
      (let* ((object-k (clj-context/read-one))
             (object-k-meta-value (clj-context/meta-value object-k))
             (object-k-leaf (replique-context/object-leaf object-k))
             (_ (clj-context/forward-comment))
             (object-v (clj-context/read-one))
             (object-v-meta-value (clj-context/meta-value object-v))
             (object-v-leaf (replique-context/object-leaf object-v)))
        (if object-k-leaf
            (progn
              (cond ((and object-v-leaf
                          (replique-context/at-for-like-let?
                           for-like? object-k-meta-value object-v-meta-value))
                     (goto-char (+ 1 (oref object-v-meta-value :start)))
                     (replique-context/handle-binding-vector target-point)
                     (goto-char (oref object-v-meta-value :end)))
                    ((and object-v-leaf
                          (> target-point (oref object-v-leaf :end)))
                     (replique-context/extract-bindings
                      target-point object-k 'replique-context/add-local-binding)
                     (goto-char (oref object-v-leaf :end)))
                    ((and (>= target-point (oref object-k-leaf :start))
                          (<= target-point (oref object-k-leaf :end)))
                     (replique-context/extract-bindings
                      target-point object-k 'replique-context/maybe-at-local-binding-position)))
              (clj-context/forward-comment))
          (setq exit t))))))

(defun replique-context/handle-let-like (target-point &optional for-like?)
  (let ((object (replique-context/extracted-value (clj-context/read-one))))
    (when (and (cl-typep object 'clj-context/object-delimited)
               (eq :vector (oref object :delimited)))
      (goto-char (+ 1 (oref object :start)))
      (clj-context/forward-comment)
      (replique-context/handle-binding-vector target-point for-like?))))

(defun replique-context/handle-named-fn-binding (object)
  (let* ((object-extracted (replique-context/extracted-value object))
         (sym (oref object-extracted :symbol))
         (meta (when (and (cl-typep object 'clj-context/object-dispatch-macro)
                          (eq :meta (oref object :dispatch-macro)))
                 (buffer-substring-no-properties (oref object :data-start)
                                                 (oref object :data-end)))))
    (when (and sym (not (string-prefix-p ":" sym)))
      `[,sym [,(oref object-extracted :start) ,(oref object-extracted :end) ,meta]])))

(defun replique-context/handle-params-bindings (target-point binding-found-fn)
  (let ((quit nil))
    (while (null quit)
      (let ((object (clj-context/read-one)))
        (if object
            (progn
              (replique-context/extract-bindings
               target-point object binding-found-fn)
              (clj-context/forward-comment))
          (setq quit t))))))

(defun replique-context/handle-fn-like (target-point)
  (let* ((object (clj-context/read-one))
         (object-extracted (replique-context/extracted-value object))
         (named-fn-binding (when (and (cl-typep object-extracted 'clj-context/object-symbol)
                                      (> target-point (oref object-extracted :end)))
                             (replique-context/handle-named-fn-binding object))))
    ;; If this is an anonymous function, move back before the binding vector
    (when (and (cl-typep object-extracted 'clj-context/object-delimited)
               (eq :vector (oref object-extracted :delimited)))
      (goto-char (oref object-extracted :start)))
    (if (and (cl-typep object-extracted 'clj-context/object-symbol)
             (<= (oref object-extracted :start) target-point (oref object-extracted :end)))
        (setq replique-context/at-binding-position? t)
      ;; do it multiple times to skip docstring and metadata map
      (let ((quit nil))
        (while (null quit)
          (clj-context/forward-comment)
          (let* ((object (clj-context/read-one))
                 (object-extracted (replique-context/extracted-value object)))
            (if (null object-extracted)
                (setq quit t)
              (cond ((and (cl-typep object-extracted 'clj-context/object-delimited)
                          (eq :vector (oref object-extracted :delimited))
                          (>= target-point (oref object-extracted :end)))
                     ;; method body (single arity)
                     (goto-char (+ 1 (oref object-extracted :start)))
                     (clj-context/forward-comment)
                     ;; don't push the named fn in locals as it is bound to a var with the same name
                     (comment
                      (when named-fn-binding
                        (puthash (aref named-fn-binding 0)
                                 (aref named-fn-binding 1)
                                 replique-context/locals)))
                     (replique-context/handle-params-bindings
                      target-point 'replique-context/add-local-binding)
                     (setq quit t))
                    ((and (cl-typep object-extracted 'clj-context/object-delimited)
                          (eq :vector (oref object-extracted :delimited))
                          (< (oref object-extracted :start)
                             target-point
                             (oref object-extracted :end)))
                     ;; point is in params vector
                     (goto-char (+ 1 (oref object-extracted :start)))
                     (clj-context/forward-comment)
                     (replique-context/handle-params-bindings
                      target-point 'replique-context/maybe-at-local-binding-position)
                     (setq quit t))
                    ((and (cl-typep object-extracted 'clj-context/object-delimited)
                          (eq :list (oref object-extracted :delimited))
                          (< (oref object-extracted :start)
                             target-point
                             (oref object-extracted :end)))
                     ;; method body (multiple arity)
                     (goto-char (+ 1 (oref object-extracted :start)))
                     (clj-context/forward-comment)
                     (let* ((object (clj-context/read-one))
                            (object-extracted (replique-context/extracted-value object)))
                       (if (null object-extracted)
                           (setq quit t)
                         (cond ((and (cl-typep object-extracted
                                               'clj-context/object-delimited)
                                     (eq :vector (oref object-extracted :delimited))
                                     (>= target-point (oref object-extracted :end)))
                                ;; method body (single arity)
                                (goto-char (+ 1 (oref object-extracted :start)))
                                (clj-context/forward-comment)
                                ;; don't push the named fn in locals as it is bound to a var
                                ;; with the same name
                                (comment
                                 (when named-fn-binding
                                   (puthash (aref named-fn-binding 0)
                                            (aref named-fn-binding 1)
                                            replique-context/locals)))
                                (replique-context/handle-params-bindings
                                 target-point 'replique-context/add-local-binding))
                               ((and (cl-typep object-extracted
                                               'clj-context/object-delimited)
                                     (eq :vector (oref object-extracted :delimited))
                                     (< (oref object-extracted :start)
                                        target-point
                                        (oref object-extracted :end)))
                                ;; point is in params vector
                                (goto-char (+ 1 (oref object-extracted :start)))
                                (clj-context/forward-comment)
                                (replique-context/handle-params-bindings
                                 target-point
                                 'replique-context/maybe-at-local-binding-position)))))
                     (setq quit t))))))))))

(defun replique-context/handle-type-forms-family (target-point fields?)
  (let* ((object (clj-context/read-one))
         (object-extracted (replique-context/extracted-value object))
         (named-fn-binding (when (and (cl-typep object-extracted 'clj-context/object-symbol)
                                      (> target-point (oref object-extracted :end)))
                             (replique-context/handle-named-fn-binding object))))
    ;; If the name of the deftype-like object could not be found, move back to the beginning of
    ;; the vector
    (when (and (cl-typep object-extracted 'clj-context/object-delimited)
               (eq :vector (oref object-extracted :delimited)))
      (goto-char (oref object-extracted :start)))
    (if (and (cl-typep object-extracted 'clj-context/object-symbol)
             (<= (oref object-extracted :start) target-point (oref object-extracted :end)))
        (when fields?
          (setq replique-context/at-binding-position? t))
      (let ((quit nil))
        ;; looking for the deftype-like fields
        ;; when fields is null (extend-type, extend-protocol ...), the named-fn-binding is not
        ;; added to the local. That's the behavior we want
        (while (and fields? (null quit))
          (clj-context/forward-comment)
          (let* ((object (clj-context/read-one))
                 (object-extracted (replique-context/extracted-value object)))
            (if (null object-extracted)
                (setq quit t)
              (cond ((and (cl-typep object-extracted 'clj-context/object-delimited)
                          (eq :vector (oref object-extracted :delimited))
                          (>= target-point (oref object-extracted :end)))
                     ;; point is after the fields vector
                     (goto-char (+ 1 (oref object-extracted :start)))
                     (clj-context/forward-comment)
                     (when named-fn-binding
                       (puthash (aref named-fn-binding 0)
                                (aref named-fn-binding 1)
                                replique-context/locals))
                     (replique-context/handle-params-bindings
                      target-point 'replique-context/add-local-binding)
                     (goto-char (+ 1 (oref object-extracted :end)))
                     (setq quit t))
                    ((and (cl-typep object-extracted 'clj-context/object-delimited)
                          (eq :vector (oref object-extracted :delimited))
                          (< (oref object-extracted :start)
                             target-point
                             (oref object-extracted :end)))
                     ;; point is in the fields vector
                     (goto-char (+ 1 (oref object-extracted :start)))
                     (clj-context/forward-comment)
                     (replique-context/handle-params-bindings
                      target-point 'replique-context/maybe-at-local-binding-position)
                     (setq quit t))))))
        (let ((quit (<= target-point (point))))
          ;; looking for the deftype-like methods
          (while (null quit)
            (clj-context/forward-comment)
            (let* ((object (clj-context/read-one))
                   (object-extracted (replique-context/extracted-value object)))
              (if (null object-extracted)
                  (setq quit t)
                (cond ((and (cl-typep object-extracted 'clj-context/object-delimited)
                            (eq :list (oref object-extracted :delimited))
                            (< (oref object-extracted :start)
                               target-point
                               (oref object-extracted :end)))
                       ;; method body
                       (goto-char (+ 1 (oref object-extracted :start)))
                       (replique-context/handle-fn-like target-point)
                       (setq quit t)))))))))))

(defun replique-context/handle-deftype-like (target-point)
  (replique-context/handle-type-forms-family target-point t))

(defun replique-context/handle-extend-type-like (target-point)
  (replique-context/handle-type-forms-family target-point nil))

(defun replique-context/handle-defmethod-like (target-point)
  (let* ((object (clj-context/read-one))
         (object-extracted (replique-context/extracted-value object)))
    ;; If the defmethod name could not be found, we already are after the dispatch val,
    ;; else read forward the dispatch val
    (cond ((and (cl-typep object-extracted 'clj-context/object-symbol)
                (<= (oref object-extracted :start) target-point (oref object-extracted :end)))
           nil)
          ((not (cl-typep object-extracted 'clj-context/object-symbol))
           (clj-context/forward-comment))
          (t (clj-context/forward-comment)
             (clj-context/read-one)
             (clj-context/forward-comment)))
    (replique-context/handle-fn-like target-point)))

(defun replique-context/handle-letfn-like (target-point)
  (let ((object (replique-context/extracted-value (clj-context/read-one))))
    (when (and (cl-typep object 'clj-context/object-delimited)
               (eq :vector (oref object :delimited))
               (> target-point (oref object :start)))
      (goto-char (+ 1 (oref object :start)))
      (clj-context/forward-comment)
      (let ((exit nil)
            (fn-like-object nil))
        (while (null exit)
          (let ((object (replique-context/extracted-value (clj-context/read-one))))
            (if object
                (when (and (cl-typep object 'clj-context/object-delimited)
                           (eq :list (oref object :delimited)))
                  (if (< (oref object :start) target-point (oref object :end))
                      (setq fn-like-object object)
                    (goto-char (+ 1 (oref object :start)))
                    (clj-context/forward-comment)        
                    (let* ((object (clj-context/meta-value (clj-context/read-one)))
                           (object-meta-value (clj-context/meta-value object)))
                      (when (and object-meta-value
                                 (cl-typep object-meta-value 'clj-context/object-symbol))
                        (let ((sym (oref object-meta-value :symbol))
                              (meta (when (and (cl-typep
                                                object 'clj-context/object-dispatch-macro)
                                               (eq :meta (oref object :dispatch-macro)))
                                      (buffer-substring-no-properties (oref object :data-start)
                                                                      (oref object :data-end)))))
                          (when (and sym (not (string-prefix-p ":" sym)))
                            (if (<= (oref object-meta-value :start)
                                    target-point
                                    (oref object-meta-value :end))
                                (setq replique-context/at-local-binding-position? t)
                              (replique-context/add-local-binding target-point sym
                                                                  (oref object-meta-value :start)
                                                                  (oref object-meta-value :end)
                                                                  meta)))))))
                  (goto-char (oref object :end)))
              (setq exit t)))
          (clj-context/forward-comment))
        (when fn-like-object
          (goto-char (+ 1 (oref fn-like-object :start)))
          (clj-context/forward-comment)
          (replique-context/handle-fn-like target-point))))))

(defun replique-context/handle-with-env-like
    (tooling-repl repl-env ns target-point object-symbol)
  (let* ((object (clj-context/read-one))
         (object-no-meta (clj-context/meta-value object)))
    (when (and (cl-typep object-no-meta 'clj-context/object-symbol)
               (> target-point (oref object-no-meta :end)))
      (let* ((resp (replique/send-tooling-msg
                    tooling-repl
                    (clj-data/hash-map :type :captured-env-locals
                                       :repl-env repl-env
                                       :ns ns
                                       :captured-env (oref object-no-meta :symbol))))
             (err (clj-data/get resp :error)))
        (if err
            (progn
              (message (clj-pprint/pprint-error-str err))
              (message "Error while getting locals from captured env: %s"
                       (oref object-no-meta :symbol)))
          (dolist (local-sym (clj-data/get resp :captured-env-locals))
            (replique-context/add-local-binding target-point (symbol-name local-sym)
                                                (oref object-symbol :start)
                                                (oref object-symbol :end)
                                                nil)))))))

(defvar replique-context/target-point nil)
(defvar replique-context/last-read nil)

(defun replique-context/pushback-read-one ()
  (let ((p (point))
        (object-leaf (replique-context/object-leaf (clj-context/read-one))))
    (goto-char p)
    object-leaf))

(defun replique-context/skip-until-target-point (&optional other-stop-condition-fn)
  (let* ((stop-recur nil)
         (object nil)
         (object-leaf nil))
    (while (null stop-recur)
      (let ((p (point)))
        (setq object (clj-context/read-one))
        (setq object-leaf (replique-context/object-leaf object))
        (if (or (null object-leaf)
                (<= replique-context/target-point (oref object-leaf :end))
                (when other-stop-condition-fn
                  (funcall other-stop-condition-fn object object-leaf)))
            (progn (setq stop-recur t)
                   (goto-char p))
          (clj-context/forward-comment))))))

(defun replique-context/keyword-then-target-point (object object-leaf)
  (let ((object-meta-value (clj-context/meta-value object)))
    (when (and (cl-typep object-meta-value 'clj-context/object-symbol)
               (string-prefix-p ":" (oref object-meta-value :symbol)))
      (clj-context/forward-comment)
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
   (let* ((object (clj-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'clj-context/object-delimited)
                       (or (eq :list (oref object-extracted :delimited))
                           (eq :vector (oref object-extracted :delimited)))
                       (> replique-context/target-point (oref object-extracted :start))
                       (< replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted)
       (goto-char (+ (oref object-extracted :start) 1))
       (clj-context/forward-comment))
     result)))

(defun replique-context/after-sequential ()
  (replique-context/restore-point-on-failure
   (let* ((object (clj-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'clj-context/object-delimited)
                       (or (eq :list (oref object-extracted :delimited))
                           (eq :vector (oref object-extracted :delimited)))
                       (>= replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted)
       (clj-context/forward-comment))
     result)))

(defun replique-context/in-keyword ()
  (replique-context/restore-point-on-failure
   (let* ((object (clj-context/read-one))
          (object-meta-value (clj-context/meta-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-meta-value 'clj-context/object-symbol)
                       (string-prefix-p ":" (oref object-meta-value :symbol))
                       (>= replique-context/target-point (oref object-meta-value :start))
                       (<= replique-context/target-point (oref object-meta-value :end)))))
     (when result
       (setq replique-context/last-read object-meta-value))
     result)))

(defun replique-context/after-keyword (&optional keywords-filter)
  (replique-context/restore-point-on-failure
   (let* ((object (clj-context/read-one))
          (object-meta-value (clj-context/meta-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-meta-value 'clj-context/object-symbol)
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
       (clj-context/forward-comment))
     result)))

(defun replique-context/in-symbol ()
  (replique-context/restore-point-on-failure
   (let* ((object (clj-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'clj-context/object-symbol)
                       (not (string-prefix-p ":" (oref object-extracted :symbol)))
                       (>= replique-context/target-point (oref object-extracted :start))
                       (<= replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted))
     result)))

(defun replique-context/after-symbol ()
  (replique-context/restore-point-on-failure
   (let* ((object (clj-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'clj-context/object-symbol)
                       (not (string-prefix-p ":" (oref object-extracted :symbol)))
                       (> replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted)
       (clj-context/forward-comment))
     result)))

(defun replique-context/in-string ()
  (replique-context/restore-point-on-failure
   (let* ((object (clj-context/read-one))
          (object-extracted (replique-context/extracted-value object))
          (object-leaf (replique-context/object-leaf object))
          (result (and object-leaf
                       (cl-typep object-extracted 'clj-context/object-string)
                       (> replique-context/target-point (oref object-extracted :start))
                       (< replique-context/target-point (oref object-extracted :end)))))
     (when result
       (setq replique-context/last-read object-extracted))
     result)))

(defun replique-context/libspec (position-libspec-option position-namespace prefix)
  (cond ((replique-context/in-symbol)
         (setq replique-context/dependency-context (clj-data/hash-map
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
                  (setq replique-context/dependency-context (clj-data/hash-map
                                                             :position position-namespace
                                                             :prefix prefix))
                  t)
                 ((replique-context/in-keyword)
                  (setq replique-context/dependency-context
                        (clj-data/hash-map :position position-libspec-option))
                  t)
                 ((replique-context/after-keyword)
                  (let ((object-keyword replique-context/last-read))
                    (when (and (equal ":refer" (oref object-keyword :symbol))
                               (replique-context/in-sequential))
                      (replique-context/skip-until-target-point)
                      (when (replique-context/in-symbol)
                        (setq replique-context/dependency-context
                              (clj-data/hash-map :position :var :namespace prefix))
                        t)))))))))

(defun replique-context/libspec-refer (namespace)
  (cond ((replique-context/in-keyword)
         (setq replique-context/dependency-context
               (clj-data/hash-map :position :libspec-option-refer))
         t)
        ((replique-context/after-keyword)
         (let ((object-keyword replique-context/last-read))
           (when (and (or (equal ":only" (oref object-keyword :symbol))
                          (equal ":exclude" (oref object-keyword :symbol)))
                      (replique-context/in-sequential))
             (replique-context/skip-until-target-point)
             (when (replique-context/in-symbol)
               (setq replique-context/dependency-context
                     (clj-data/hash-map :position :var :namespace namespace))
               t))))))

(defun replique-context/import-spec ()
  (cond ((replique-context/in-symbol)
         (setq replique-context/dependency-context
               (clj-data/hash-map :position :package-or-class))
         t)
        ((replique-context/in-sequential)
         (cond ((replique-context/in-symbol)
                (setq replique-context/dependency-context
                      (clj-data/hash-map :position :package-or-class))
                t)
               ((replique-context/after-symbol)
                (let ((object-package replique-context/last-read))
                  (replique-context/skip-until-target-point)
                  (when (replique-context/in-symbol)
                    (setq replique-context/dependency-context
                          (clj-data/hash-map :position :class
                                             :package (oref object-package :symbol)))
                    t)))))))

(defun replique-context/walk-ns (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (replique-context/skip-until-target-point)
     (when (replique-context/in-sequential)
       (cond ((replique-context/in-keyword)
              (setq replique-context/dependency-context
                    (clj-data/hash-map :position :dependency-type))
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
                             (clj-data/hash-map :position position-namespace :prefix ""))
                       t)
                      ((replique-context/in-sequential)
                       (replique-context/libspec position-libspec-option position-namespace ""))
                      ((replique-context/in-keyword)
                       (setq replique-context/dependency-context
                             (clj-data/hash-map :position :flag))
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
                      (clj-data/hash-map :position :load-path))
                t)))))))

(defun replique-context/walk-require (target-point use? require-macros?)
  (let ((replique-context/target-point target-point)
        (position-libspec-option (if use? :libspec-option-refer :libspec-option))
        (position-namespace (if require-macros? :namespace-macros :namespace)))
    (replique-context/restore-point-on-failure
     (cond ((replique-context/in-symbol)
            (setq replique-context/dependency-context
                  (clj-data/hash-map :position position-namespace
                                     :prefix ""))
            t)
           ((replique-context/in-sequential)
            (replique-context/libspec position-libspec-option position-namespace ""))
           ((or (replique-context/after-sequential) (replique-context/after-symbol))
            (when (replique-context/in-keyword)
              (setq replique-context/dependency-context (clj-data/hash-map :position :flag))
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
       (setq replique-context/dependency-context (clj-data/hash-map :position :load-path))
       t))))

(defun replique-context/walk-refer (target-point)
  (let ((replique-context/target-point target-point))
    (replique-context/restore-point-on-failure
     (cond ((replique-context/in-symbol)
            (setq replique-context/dependency-context (clj-data/hash-map :position :namespace))
            t)
           ((replique-context/after-symbol)
            (let* ((namespace-object replique-context/last-read)
                   (namespace (oref namespace-object :symbol)))
              (replique-context/libspec-refer namespace)))))))

(comment
 (let* ((forward-point (replique-context/walk-init)))
   (goto-char (+ 1 (point)))
   (clj-context/forward-comment)
   (replique-context/walk-ns forward-point)
   (clj-pprint/pprint-str replique-context/dependency-context))
 )

(comment
 (let ((clj-context/splice-ends '())
       (clj-context/platform-tag ":clj"))
   (clj-context/read-one))
 )

(defun replique-context/unwrap-comment (repl-type target-point)
  (let ((clj-context/splice-ends '())
        (clj-context/platform-tag (symbol-name repl-type)))
    (let ((p-start (point))
          (object-meta-value (clj-context/meta-value (clj-context/read-one)))
          (p-end (point)))
      (if (and (cl-typep object-meta-value 'clj-context/object-delimited)
               (eq :list (oref object-meta-value :delimited))
               (< target-point (oref object-meta-value :end)))
          (progn
            (goto-char (+ 1 (oref object-meta-value :start)))
            (clj-context/forward-comment)
            (let ((object-meta-value (clj-context/meta-value (clj-context/read-one))))
              (clj-context/forward-comment)
              (if (and (cl-typep object-meta-value 'clj-context/object-symbol)
                       (seq-contains replique/clj-comment (oref object-meta-value :symbol))
                       (> target-point (point)))
                  (let ((replique-context/target-point target-point))
                    (replique-context/skip-until-target-point)
                    (let ((p-start (point)))
                      (clj-context/read-one)
                      (let ((p-end (point)))
                        (when (> p-end p-start)
                          `(,p-start . ,p-end)))))
                (when (> p-end p-start)
                  `(,p-start . ,p-end)))))
        (when (> p-end p-start)
          `(,p-start . ,p-end))))))

(defun replique-context/handle-contextual-call
    (tooling-repl repl-env ns target-point context-forms object-symbol)
  (let* ((p (point))
         (symbol (oref object-symbol :symbol))
         (binding-context (clj-data/get
                           (clj-data/get context-forms :binding-context)
                           symbol))
         (dependency-context (clj-data/get
                              (clj-data/get context-forms :dependency-context)
                              symbol))
         (ns-context (clj-data/get
                      (clj-data/get context-forms :ns-context)
                      symbol)))
    (cond ((equal binding-context :let-like)
           (replique-context/handle-let-like target-point)
           (goto-char p))
          ((equal binding-context :for-like)
           (replique-context/handle-let-like target-point t)
           (goto-char p))
          ((equal binding-context :fn-like)
           (replique-context/handle-fn-like target-point)
           (goto-char p))
          ((equal binding-context :deftype-like)
           ;; deftype-like methods are not closures
           (setq replique-context/locals (clj-data/hash-map))
           (replique-context/handle-deftype-like target-point)
           (goto-char p))
          ((equal binding-context :extend-type-like)
           (replique-context/handle-extend-type-like target-point)
           (goto-char p))
          ((equal binding-context :defmethod-like)
           (replique-context/handle-defmethod-like target-point)
           (goto-char p))
          ((equal binding-context :letfn-like)
           (replique-context/handle-letfn-like target-point)
           (goto-char p))
          ((equal binding-context :with-env-like)
           (replique-context/handle-with-env-like
            tooling-repl repl-env ns
            target-point object-symbol)
           (goto-char p))
          ((equal ns-context :ns-like)
           (when (replique-context/walk-ns target-point)
             (setq replique-context/in-ns-form? t)))
          ((equal dependency-context :require-like)
           (replique-context/walk-require target-point nil nil))
          ((equal dependency-context :use-like)
           (replique-context/walk-require target-point t nil))
          ((equal dependency-context :require-macros-like)
           (replique-context/walk-require target-point nil t))
          ((equal dependency-context :import-like)
           (replique-context/walk-import target-point))
          ((equal dependency-context :refer-like)
           (replique-context/walk-refer target-point))
          ((equal dependency-context :refer-clojure-like)
           (replique-context/walk-refer-clojure target-point))
          ((equal dependency-context :load-like)
           (replique-context/walk-load target-point)))))

(defun replique-context/handle-fn-context (target-point innermost-fn-object)
  (when innermost-fn-object
    (goto-char (+ 1 (oref innermost-fn-object :start)))
    (clj-context/forward-comment)
    (let* ((fn-object (clj-context/read-one))
           (fn-object-no-meta (clj-context/meta-value fn-object)))
      (setq replique-context/fn-context (oref fn-object-no-meta :symbol))
      (when (> target-point (oref fn-object-no-meta :end))
        (setq replique-context/fn-context-position (+ 1 replique-context/fn-context-position))))
    (clj-context/forward-comment)
    (let* ((param-object (clj-context/read-one))
           (param-meta-value (clj-context/meta-value param-object))
           (param-sym (cond ((cl-typep param-meta-value 'clj-context/object-symbol)
                             (oref param-meta-value :symbol))
                            ((cl-typep param-meta-value 'clj-context/object-string)
                             (buffer-substring-no-properties
                              (oref param-meta-value :start) (oref param-meta-value :end)))
                            ((cl-typep param-meta-value 'clj-context/object-delimited)
                             (buffer-substring-no-properties
                              (oref param-meta-value :start) (oref param-meta-value :end))))))
      (when param-sym
        (let ((param-meta (when (and
                                 (cl-typep param-object 'clj-context/object-dispatch-macro)
                                 (eq :meta (oref param-object :dispatch-macro)))
                            (buffer-substring-no-properties (oref param-object :data-start)
                                                            (oref param-object :data-end)))))
          (setq replique-context/fn-param param-sym)
          (setq replique-context/fn-param-meta param-meta))))
    (let ((quit nil))
      (while (null quit)
        (if (> target-point (point))
            (setq replique-context/fn-context-position (+ 1 replique-context/fn-context-position))
          (setq quit t))
        (clj-context/forward-comment)
        (let ((object (clj-context/read-one)))
          (when (null object)
            (setq quit t)))))
    (when (> target-point (point))
      (replique-context/reset-state))))

;; Errors (read-one returning nil) may prevent the computation of some contextual values
;; such as the fn-context. In such a case, we reset the contextual states and go
;; to the target-point
(defun replique-context/walk
    (tooling-repl repl-env ns target-point context-forms)
  (let ((innermost-fn-object nil))
    (while (< (point) target-point)
      (let* ((object (clj-context/read-one))
             (object-leaf (replique-context/object-leaf object)))
        (if object-leaf
            (progn
              (cond ((and (cl-typep object-leaf 'clj-context/object-delimited)
                          (eq :list (oref object-leaf :delimited))
                          (> target-point (oref object-leaf :start))
                          (< target-point (oref object-leaf :end)))
                     (goto-char (+ (oref object-leaf :start) 1))
                     (clj-context/forward-comment)
                     (let* ((p (point))
                            (maybe-fn (clj-context/read-one))
                            (maybe-fn-no-meta (clj-context/meta-value maybe-fn)))
                       (if (and (cl-typep maybe-fn-no-meta 'clj-context/object-symbol)
                                (not (string-prefix-p ":" (oref maybe-fn-no-meta :symbol))))
                           (progn
                             (clj-context/forward-comment)
                             (replique-context/handle-contextual-call
                              tooling-repl repl-env ns
                              target-point context-forms maybe-fn-no-meta)
                             (setq innermost-fn-object object-leaf))
                         (goto-char p))))
                    ((and (cl-typep object-leaf 'clj-context/object-delimited)
                          (> target-point (oref object-leaf :start))
                          (< target-point (oref object-leaf :end)))
                     (goto-char (+ (oref object-leaf :start) 1))))
              (clj-context/forward-comment))
          (replique-context/reset-state)
          (setq innermost-fn-object nil)
          (goto-char target-point))))
    (replique-context/handle-fn-context target-point innermost-fn-object)))

(defun replique-context/maybe-skip-read-discard-or-splice-forward ()
  (while (or (and (equal (char-after (point)) ?#)
                  (equal (char-after (+ 1 (point))) ?_))
             (and (equal (char-after (point)) ?~)
                  (equal (char-after (+ 1 (point))) ?@)))
    (forward-char 2)))

(defun replique-context/maybe-skip-symbol-prefix ()
  (let ((continue t))
    (while continue
      (let ((skip-nb (cond ((or (and (equal (char-after (point)) ?#)
                                     (equal (char-after (+ 1 (point))) ?_))
                                (and (equal (char-after (point)) ?#)
                                     (equal (char-after (+ 1 (point))) ?'))
                                (and (equal (char-after (point)) ?~)
                                     (equal (char-after (+ 1 (point))) ?@)))
                            2)
                           ((or (equal (char-after (point)) ?')
                                (equal (char-after (point)) ?`)
                                (equal (char-after (point)) ?~))
                            1))))
        (if (null skip-nb)
            (setq continue nil)
          (forward-char skip-nb))))))

(defun replique-context/walk-init (target-point)
  (let* ((ppss (clj-context/syntax-ppss target-point))
         (top-level (syntax-ppss-toplevel-pos ppss)))
    (replique-context/reset-state)
    (when top-level
      (goto-char top-level)
      (clj-context/maybe-skip-dispatch-macro-or-quoted-backward)
      (clj-context/forward-comment)
      ppss)))

;; Same as clojure-namespace-name-regex but without the start-line check
(defconst replique-context/clojure-namespace-name-regex
  (rx "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "{}[]()\"" whitespace))) symbol-end)))


(defvar-local replique-context/ns-starts nil)
(defvar-local replique-context/buffer-ns-starts-modified-tick nil)

(defun replique-context/clojure-find-ns-handle-ns-form (the-ns ns-start-pos)
  (setq replique-context/ns-starts
        (cons `(,the-ns . ,ns-start-pos) replique-context/ns-starts))
  (goto-char ns-start-pos)
  (replique-context/clojure-find-ns-starts** nil 0))

(defun replique-context/clojure-find-ns-handle-in-ns-form (the-ns ns-start-pos previous-depth)
  (let ((depth (car (syntax-ppss (point)))))
    (if (eq 0 depth)
        (progn
          (setq replique-context/ns-starts
                (cons `(,the-ns . ,ns-start-pos) replique-context/ns-starts))
          (goto-char ns-start-pos)
          (replique-context/clojure-find-ns-starts** nil previous-depth))
      (parse-partial-sexp (point) (point-max) -1)
      (let ((ns-end-pos (point))
            (previous-ns (caar replique-context/ns-starts)))
        (setq replique-context/ns-starts
              (cons `(,the-ns . ,ns-start-pos) replique-context/ns-starts))
        (goto-char ns-start-pos)
        (replique-context/clojure-find-ns-starts** ns-end-pos depth)
        (when (> depth previous-depth)
          (setq replique-context/ns-starts
                (cons `(,previous-ns . ,ns-end-pos)
                      replique-context/ns-starts))
          (replique-context/clojure-find-ns-starts** nil previous-depth))))))

(defun replique-context/clojure-find-ns-starts** (bound previous-depth)
  (let ((ns-start-pos (re-search-forward
                       replique-context/clojure-namespace-name-regex bound t)))
    (when ns-start-pos
      (let ((the-ns (match-string-no-properties 4))
            (in-ns? (match-string-no-properties 2))
            (form (match-string-no-properties 0)))
        (when the-ns
          (backward-char (length form))
          (if in-ns?
              (replique-context/clojure-find-ns-handle-in-ns-form
               the-ns ns-start-pos previous-depth)
            (let ((depth (car (syntax-ppss (point)))))
              (if (eq 0 depth)
                  (replique-context/clojure-find-ns-handle-ns-form
                   the-ns ns-start-pos)
                (goto-char ns-start-pos)
                (replique-context/clojure-find-ns-starts** bound previous-depth)))))))))

(defun replique-context/clojure-find-ns-starts* (bound)
  (replique-context/clojure-find-ns-starts** bound 0)
  (setq replique-context/ns-starts (nreverse replique-context/ns-starts))
  (when (car replique-context/ns-starts)
    (setcdr (car replique-context/ns-starts) 0))
  replique-context/ns-starts)

(defun replique-context/clojure-find-ns-starts ()
  (let ((modified-tick (buffer-chars-modified-tick)))
    (if (equal modified-tick replique-context/buffer-ns-starts-modified-tick)
        replique-context/ns-starts
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (setq replique-context/ns-starts nil)
          (setq replique-context/ns-starts (replique-context/clojure-find-ns-starts* nil))
          (setq replique-context/buffer-ns-starts-modified-tick modified-tick)
          replique-context/ns-starts)))))

(defun replique-context/clojure-find-ns ()
  (let ((ns-starts (replique-context/clojure-find-ns-starts))
        (continue t)
        (ns-candidate nil))
    (while continue
      (let ((ns-start (car ns-starts)))
        (if (and ns-start (>= (point) (cdr ns-start)))
            (progn
              (setq ns-candidate (car ns-start))
              (setq ns-starts (cdr ns-starts)))
          (setq continue nil)
          ns-candidate)))
    ns-candidate))

(defun replique-context/symbol-at-point ()
  (save-excursion
    (let ((p (point)))
      (skip-chars-backward clj-context/symbol-separator-re)
      (replique-context/maybe-skip-symbol-prefix)
      (let ((start (point)))
        (when (<= start p)
          (skip-chars-forward clj-context/symbol-separator-re)
          (when (> (point) start)
            (buffer-substring-no-properties start (point))))))))

(defvar replique-context/context nil)

(defun replique-context/get-context (tooling-repl ns repl-env)
  (or replique-context/context
      (let* ((resp (replique/send-tooling-msg
                    tooling-repl
                    (clj-data/hash-map :type :context
                                       :repl-env repl-env
                                       :ns ns))))
        (let ((err (clj-data/get resp :error)))
          (if err
              (progn
                (message "%s" (clj-pprint/pprint-error-str err))
                (message "context failed"))
            (save-excursion
              (let* ((target-point (point))
                     (ppss (replique-context/walk-init target-point))
                     (repl-type (clj-data/get resp :repl-type)))
                (when target-point
                  (let ((clj-context/platform-tag
                         (symbol-name repl-type))
                        (clj-context/splice-ends '()))
                    (replique-context/walk tooling-repl repl-env ns target-point resp))
                  (setq replique-context/context
                        (clj-data/hash-map
                         :locals replique-context/locals
                         :at-binding-position? replique-context/at-binding-position?
                         :at-local-binding-position? replique-context/at-local-binding-position?
                         :in-string? (not (null (nth 3 ppss)))
                         :in-comment? (not (null (nth 4 ppss)))
                         :in-ns-form? replique-context/in-ns-form?
                         :dependency-context replique-context/dependency-context
                         :fn-context replique-context/fn-context
                         :fn-context-position replique-context/fn-context-position
                         :fn-param replique-context/fn-param
                         :fn-param-meta replique-context/fn-param-meta))
                  replique-context/context))))))))

(defun replique-context/reset-cache ()
  (setq replique-context/context nil))

(add-hook 'post-command-hook 'replique-context/reset-cache)

(provide 'replique-context)
