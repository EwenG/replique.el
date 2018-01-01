;; replique.el ---   -*- lexical-binding: t; -*-

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

;; Version 0.0.9-SNAPSHOT
;; Package-Requires: ((emacs "25") (clojure-mode "5.6.0"))

;; Commentary:

;; Code:

(require 'subr-x)
(require 'comint)
(require 'clojure-mode)
(require 'replique-edn)
(require 'replique-hashmap)
(require 'replique-resources)
(require 'map)
(require 'replique-omniscient)
(require 'replique-lein)
(require 'replique-remove-var)
(require 'replique-transit)
(require 'replique-context)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/keyword-to-string (k)
  (substring (symbol-name k) 1))

(defun replique/message-nolog (format-string)
  (let ((message-log-max nil))
    (message "%s" format-string)))

(defun replique/visible-buffers ()
  (let ((buffers '()))
    (walk-windows
     (lambda (window) (push (window-buffer window) buffers))
     nil 'visible)
    buffers))

(defun replique/symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))

;; completing-read uses an alphabetical order by default. This function can be used too keep
;; the order of the candidates
(defun replique/presorted-completion-table (completions)
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,'identity))
      (complete-with-action action completions string pred))
    completions))

(defvar replique/repls nil)

(defun replique/pp-repls ()
  (string-join
   (thread-last replique/repls
     (mapcar (lambda (repl)
               (let* ((repl (if (replique/contains? repl :buffer)
                                (replique/assoc repl :buffer '**)
                              repl))
                      (repl (if (replique/contains? repl :proc)
                                (replique/assoc repl :proc '**)
                              repl))
                      (repl (if (replique/contains? repl :network-proc)
                                (replique/assoc repl :network-proc '**)
                              repl)))
                 repl)))
     (mapcar 'replique-edn/pr-str))
   "\n"))

(defun replique/plist->alist (plist)
  (let ((alist '()))
    (while plist
      (setq alist (push `(,(car plist) . ,(cadr plist)) alist))
      (setq plist (cddr plist)))
    alist))

(defun replique/update-repl (old-repl updated-repl)
  (setq replique/repls (mapcar (lambda (repl)
                                 (if (eq repl old-repl)
                                     updated-repl
                                   repl))
                               replique/repls))
  updated-repl)

(defun replique/repls-or-repl-by (filtering-fn matching-fn source &rest args)
  (let* ((pred (lambda (repl)
                 (seq-every-p (lambda (arg)
                                (let ((k (car arg))
                                      (v (cdr arg)))
                                  (or (equal :error-on-nil k)
                                      (and (replique/contains? repl k)
                                           (funcall matching-fn (replique/get repl k) v)))))
                              (replique/plist->alist args))))
         (found (funcall filtering-fn pred source)))
    (if (and (null found) (plist-get args :error-on-nil))
        (user-error "No started REPL")
      found)))

(defun replique/repl-by (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-find 'equal replique/repls args))

(defun replique/repls-by (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-filter 'equal replique/repls args))

(defun replique/active-repl (repl-type-or-types &optional error-on-nil)
  ;; filter the repls to only keep the repls of the active process
  (let* ((active-process-dir (thread-first
                                 (replique/repl-by :repl-type :tooling :error-on-nil error-on-nil)
                               (replique/get :directory)))
         (replique/repls (if active-process-dir
                             (seq-filter (lambda (repl)
                                           (string= active-process-dir
                                                    (replique/get repl :directory)))
                                         replique/repls))))
    (if (listp repl-type-or-types)
        (apply 'replique/repls-or-repl-by 'seq-find
               (lambda (repl-v repl-type-or-types)
                 (member repl-v repl-type-or-types))
               replique/repls (list :repl-type repl-type-or-types))
      (replique/repl-by :repl-type repl-type-or-types :error-on-nil error-on-nil))))

(defun replique/guess-project-root-dir ()
  (or (locate-dominating-file default-directory "project.clj")
      default-directory))

(defmacro replique/with-modes-dispatch (&rest modes-alist)
  (let* ((tooling-repl-sym (make-symbol "tooling-repl"))
         (clj-repl-sym (make-symbol "clj-repl-sym"))
         (cljs-repl-sym (make-symbol "cljs-repl-sym"))
         (active-repl-sym (make-symbol "active-repl-sym"))
         (dispatch-code
          (mapcar
           (lambda (item)
             (let ((m (car item))
                   (f (cdr item)))
               ;; The order of priority is the order of the modes as defined during
               ;; the use of the macro
               (cond ((equal 'clojure-mode m)
                      `((equal 'clojure-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,clj-repl-sym)))
                     ((equal 'clojurescript-mode m)
                      `((equal 'clojurescript-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'clojurec-mode m)
                      `((equal 'clojurec-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,active-repl-sym)))
                     ((equal 'css-mode m)
                      `((equal 'css-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'js2-mode m)
                      `((equal 'js2-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'sass-mode m)
                      `((equal 'sass-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'scss-mode m)
                      `((equal 'scss-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'less-css-mode m)
                      `((equal 'less-css-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'stylus-mode m)
                      `((equal 'stylus-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal '-mode m)
                      `((equal 'less-mode major-mode)
                        (funcall ,f ,tooling-repl-sym ,cljs-repl-sym)))
                     ((equal 'replique/mode m)
                      `((equal 'replique/mode major-mode)
                        (funcall ,f (replique/repl-by :buffer (current-buffer)))))
                     ((equal 't m)
                      `(t ,f)))))
           modes-alist)))
    `(let* ((,tooling-repl-sym (replique/active-repl :tooling t))
            (,clj-repl-sym (replique/active-repl :clj))
            (,cljs-repl-sym (replique/active-repl :cljs))
            (,active-repl-sym (replique/active-repl '(:clj :cljs))))
       (cond ,@dispatch-code))))

;; Auto completion

(defconst replique/annotations-map
  (replique/hash-map
   :method "me"
   :field "fi"
   :static-method "sm"
   :static-field "sf"
   :keyword "k"
   :local "l"
   :namespace "n"
   :class "c"
   :macro "m"
   :function "f"
   :var "v"
   :resource "r"
   :special-form "s"
   :directory "d"))

(defun replique/make-candidate (c)
  (map-let
      ((:candidate candidate) (:type type) (:package package) (:ns ns) (:match-index match-index))
      c
    (propertize candidate
                'meta (replique/hash-map :type type :package package :ns ns)
                'match-index match-index)))

(defun replique/auto-complete* (prefix tooling-repl repl-env ns)
  (let* ((resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :completion
                                   :repl-env repl-env
                                   :context (replique-context/get-context repl-env)
                                   :ns ns
                                   :prefix prefix))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-edn/pr-str err))
            (message "completion failed with prefix %s" prefix)
            nil)
        (let* ((candidates (replique/get resp :candidates)))
          (mapcar 'replique/make-candidate candidates))))))

(defun replique/auto-complete-session (prefix repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-env (replique/get repl :repl-env))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/auto-complete* prefix tooling-repl repl-env ns)))

(defun replique/auto-complete-clj (prefix tooling-repl clj-repl)
  (when clj-repl
    (replique/auto-complete* prefix tooling-repl
                             (replique/get clj-repl :repl-env)
                             (replique-context/clojure-find-ns))))

(defun replique/auto-complete-cljs (prefix tooling-repl cljs-repl)
  (when cljs-repl
    (replique/auto-complete* prefix tooling-repl
                             (replique/get cljs-repl :repl-env)
                             (replique-context/clojure-find-ns))))

(defun replique/auto-complete-cljc (prefix tooling-repl repl)
  (when repl
    (replique/auto-complete* prefix tooling-repl
                             (replique/get repl :repl-env)
                             (replique-context/clojure-find-ns))))

(defun replique/auto-complete (prefix)
  (when (not (null (replique/active-repl :tooling)))
    (replique/with-modes-dispatch
     (replique/mode . (apply-partially 'replique/auto-complete-session prefix))
     (clojure-mode . (apply-partially 'replique/auto-complete-clj prefix))
     (clojurescript-mode . (apply-partially 'replique/auto-complete-cljs prefix))
     (clojurec-mode . (apply-partially 'replique/auto-complete-cljc prefix))
     (t . nil))))

(defun replique/auto-complete-annotation (candidate)
  (when-let ((meta (get-text-property 0 'meta candidate)))
    (map-let ((:type type) (:package package) (:ns ns)) meta
      (let ((type (gethash type replique/annotations-map)))
        (cond ((and ns type)
               (format "%s <%s>" ns type))
              ((and package type)
               (format "%s <%s>" package type))
              (type
               (format "<%s>" type))
              (ns
               (format "%s" ns))
              (package
               (format "%s" package))
              (t nil))))))

(defun replique/auto-complete-match-index (candidate)
  (get-text-property 0 'match-index candidate))

(defun replique/repliquedoc-format-arglist (index arglist)
  (let ((args-index 1)
        (force-highlight nil))
    (thread-first
        (lambda (arg)
          (cond ((equal "&" arg)
                 (when (<= args-index index)
                   (setq force-highlight t))
                 arg)
                ((or force-highlight (equal index args-index))
                 (setq args-index (1+ args-index))
                 (replique-edn/with-face :object arg :face 'eldoc-highlight-function-argument))
                (t
                 (setq args-index (1+ args-index))
                 arg)))
      (mapcar arglist)
      (seq-into 'vector))))

(defun replique/repliquedoc-format-arglists (index arglists)
  (thread-first (lambda (x) (replique/repliquedoc-format-arglist index x))
    (mapcar arglists)
    replique-edn/print-str))

(comment
 (replique/repliquedoc-format-arglists 0 '([">main-input" (replique/hash-map :keys ["uid" event data reply-fn] :as msg)]))
 )

(defun replique/repliquedoc-format (msg)
  (map-let ((:name name) (:arglists arglists) (:return return) (:index index)) msg
    (cond ((and arglists return)
           (format "%s: %s -> %s"
                   name (replique/repliquedoc-format-arglists index arglists) return))
          (arglists
           (format "%s: %s" name (replique/repliquedoc-format-arglists index arglists)))
          (name (format "%s" name))
          (t nil))))

(defun replique/repliquedoc* (tooling-repl repl-env ns)
  (let ((resp (replique/send-tooling-msg
               tooling-repl
               (replique/hash-map :type :repliquedoc
                                  :repl-env repl-env
                                  :context (replique-context/get-context repl-env)
                                  :ns ns))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-edn/pr-str err))
            (message "eldoc failed")
            nil)
        (with-demoted-errors "eldoc error: %s"
          (replique/repliquedoc-format (replique/get resp :doc)))))))

(defun replique/repliquedoc-session (repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-env (replique/get repl :repl-env))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/repliquedoc* tooling-repl repl-env ns)))

(defun replique/repliquedoc-clj (tooling-repl clj-repl)
  (when clj-repl
    (replique/repliquedoc*
     tooling-repl (replique/get clj-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/repliquedoc-cljs (tooling-repl cljs-repl)
  (when cljs-repl
    (replique/repliquedoc*
     tooling-repl (replique/get cljs-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/repliquedoc-cljc (tooling-repl repl)
  (when repl
    (replique/repliquedoc*
     tooling-repl (replique/get repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/eldoc-documentation-function ()
  ;; Ensures a REPL is started, since eldoc-mode is enabled globally by default
  (when (not (null (replique/active-repl :tooling)))
    (replique/with-modes-dispatch
     (replique/mode . 'replique/repliquedoc-session)
     (clojure-mode . 'replique/repliquedoc-clj)
     (clojurescript-mode . 'replique/repliquedoc-cljs)
     (clojurec-mode . 'replique/repliquedoc-cljc)
     (t . nil))))

(defun replique/in-ns-clj (ns-name tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/send-input-from-source-clj
     (format "(clojure.core/in-ns '%s)" ns-name) tooling-repl clj-repl)))

(defun replique/in-ns-cljs (ns-name tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/send-input-from-source-cljs
     (format "(replique.interactive/cljs-in-ns '%s)" ns-name)
     tooling-repl cljs-repl)))

(defun replique/in-ns-cljc (ns-name tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique/send-input-from-source-cljc
     (if (equal :cljs (replique/get repl :repl-type))
         (format "(replique.interactive/cljs-in-ns '%s)" ns-name)
       (format "(clojure.core/in-ns '%s)" ns-name))
     tooling-repl repl)))

(defun replique/in-ns (ns-name)
  "Change the active REPL namespace"
  (interactive (replique/symprompt "Set ns to" (replique-context/clojure-find-ns)))
  (replique/with-modes-dispatch
   (clojure-mode . (apply-partially 'replique/in-ns-clj ns-name))
   (clojurescript-mode . (apply-partially 'replique/in-ns-cljs ns-name))
   (clojurec-mode . (apply-partially 'replique/in-ns-cljc ns-name))
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(defun replique/symbol-backward ()
  (let* ((ppss (syntax-ppss (point)))
         (in-string? (not (null (nth 3 ppss)))))
    (if in-string?
        (let ((string-start (nth 8 ppss)))
          (when (and string-start
                     (< (+ 1 string-start) (point))
                     (>= string-start (line-beginning-position)))
            (buffer-substring-no-properties (+ 1 string-start) (point))))
      (let ((sym-bounds (bounds-of-thing-at-point 'symbol)))
        (when sym-bounds
          (buffer-substring-no-properties (car sym-bounds) (point)))))))

(defmacro replique/temporary-invisible-change (&rest forms)
  "Executes FORMS with a temporary buffer-undo-list, undoing on return.
The changes you make within FORMS are undone before returning.
But more importantly, the buffer's buffer-undo-list is not affected.
This allows you to temporarily modify read-only buffers too."
  `(let* ((buffer-undo-list)
          (modified (buffer-modified-p))
          (inhibit-read-only t)
          (temporary-res nil)
          (temporary-point (point)))
     (unwind-protect
         (setq temporary-res (progn ,@forms))
       (primitive-undo (length buffer-undo-list) buffer-undo-list)
       (set-buffer-modified-p modified)
       (goto-char temporary-point))
     temporary-res))

(defun replique/strip-text-properties(txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun replique/form-with-prefix* ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (replique/temporary-invisible-change
     (progn
       (when bounds (delete-region (car bounds) (cdr bounds)))
       (insert "__prefix__")
       (thing-at-point 'defun t)))))

;; Execute in a temp buffer because company-mode expects the current buffer
;; to not change at all
(defun replique/form-with-prefix ()
  (let ((defun-bounds (bounds-of-thing-at-point 'defun)))
    (when defun-bounds
      (let* ((point-offset-backward (- (cdr defun-bounds) (point)))
             (defun-content (buffer-substring (car defun-bounds)
                                              (cdr defun-bounds))))
        (with-temp-buffer
          (clojure-mode-variables)
          (set-syntax-table clojure-mode-syntax-table)
          (insert defun-content)
          (backward-char point-offset-backward)
          (replique/form-with-prefix*))))))

(defun replique/company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cond
   ((equal command 'interactive)
    (company-begin-backend 'replique/company-backend))
   ((equal command 'prefix) (when (or (derived-mode-p 'clojure-mode)
                                      (derived-mode-p 'replique/mode))
                              (replique/symbol-backward)))
   ((equal command 'sorted) t)
   ((equal command 'candidates) (replique/auto-complete arg))
   ((equal command 'annotation) (replique/auto-complete-annotation arg))
   ((equal command 'no-cache) t)
   ((equal command 'match) (replique/auto-complete-match-index arg))))

(defun replique/comint-is-closed-sexpr (start limit)
  (let* ((parser-state (parse-partial-sexp start limit))
         (depth (car parser-state))
         (in-string? (nth 3 parser-state)))
    (and (equal depth 0) (null in-string?))))

(defvar replique/eval-from-source-meta (replique/hash-map :url nil
                                                          :line nil
                                                          :column nil))

(defun replique/comint-input-sender (proc string)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/assoc replique/eval-from-source-meta
                                :type :source-meta
                                :repl-env :replique/clj))))
    (let ((err (replique/get resp :error)))
      (when err
        (message "%s" (replique-edn/pr-str err))
        (message "source-meta failed"))
      (comint-simple-send proc string))))

(defun replique/comint-send-input ()
  "Send the pending comint buffer input to the comint process if the pending input is well formed and point is after the prompt"
  (interactive)
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc))
             (comint-input-sender 'replique/comint-input-sender))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;; Point is before the prompt. Do nothing.
              (t nil))))))

(defmacro replique/return-nil-on-quit (&rest body)
  (let ((ret-sym (make-symbol "ret-sym")))
    `(let ((inhibit-quit t))
       (let ((,ret-sym (with-local-quit ,@body)))
         (setq quit-flag nil)
         ,ret-sym))))

(defun replique/comint-kill-input ()
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (> (point) (marker-position pmark))
        (let ((killed (buffer-substring-no-properties pmark (point))))
          (kill-region pmark (point))
          killed)
      "")))

(defun replique/comint-send-input-from-source (input)
  (let ((process (get-buffer-process (current-buffer))))
    (when (not process)
      (user-error "Current buffer has no process"))
    (goto-char (point-max))
    (let ((old-input (replique/comint-kill-input)))
      (goto-char (process-mark process))
      (insert input)
      (replique/comint-send-input)
      (goto-char (process-mark process))
      (insert old-input))))

(defun replique/send-input-from-source-session (input repl)
  (let ((buff (replique/get repl :buffer)))
    (with-current-buffer buff
      (replique/comint-send-input-from-source input))))

(defun replique/send-input-from-source-clj (input tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (let ((buff (replique/get clj-repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source input)))))

(defun replique/send-input-from-source-cljs (input tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (let ((buff (replique/get cljs-repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source input)))))

(defun replique/send-input-from-source-cljc
    (input tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((buff (replique/get repl :buffer)))
      (with-current-buffer buff
        (replique/comint-send-input-from-source input)))))

(defun replique/send-input-from-source-dispatch (input)
  (replique/with-modes-dispatch
   (replique/mode . (apply-partially 'replique/send-input-from-source-session input))
   (clojure-mode . (apply-partially 'replique/send-input-from-source-clj input))
   (clojurescript-mode . (apply-partially'replique/send-input-from-source-cljs input))
   (clojurec-mode . (apply-partially'replique/send-input-from-source-cljc input))
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(defun replique/maybe-change-ns ()
  (let ((repl-type (cond ((equal major-mode 'clojure-mode) :clj)
                         ((equal major-mode 'clojurescript-mode) :cljs)
                         ((equal major-mode 'clojurec-mode) '(:clj :cljs))
                         (t nil))))
    (when repl-type
      (let* ((active-repl (replique/active-repl repl-type))
             (repl-ns (replique/get active-repl :ns))
             (ns (replique-context/clojure-find-ns)))
        (when (and ns repl-ns (not (string= ns (symbol-name repl-ns))))
          (replique/in-ns ns))))))

(defun replique/column-number-at-pos (&optional pos)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (or pos (point)))
      (current-column))))

(defun replique/line-number-at-pos (&optional pos)
  (save-restriction
    (widen)
    (let ((opoint (or pos (point))) start)
      (save-excursion
        (goto-char (point-min))
        (setq start (point))
        (goto-char opoint)
        (forward-line 0)
        (1+ (count-lines start (point)))))))

(defun replique/eval-region (start end p)
  "Eval the currently highlighted region"
  (interactive "r\nP")
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (input (filter-buffer-substring start end)))
    (when (> (length input) 0)
      (replique/maybe-change-ns)
      (let* ((line (replique/line-number-at-pos start))
             (column (1+ (replique/column-number-at-pos start)))
             (replique/eval-from-source-meta (replique/hash-map
                                              :line line
                                              :column column
                                              :url (replique/buffer-url
                                                    (buffer-file-name)))))
        (if p
            (replique/send-input-from-source-dispatch
             (concat "(replique.omniscient/with-redefs " input ")"))
          (replique/send-input-from-source-dispatch input))))))

(defun replique/eval-last-sexp (p)
  "Eval the previous sexp"
  (interactive "P")
  (replique/eval-region
   (save-excursion
     (clojure-backward-logical-sexp 1) (point))
   (point)
   p))

;; if the cursor is in a string, thing-at-point 'sexp does not return the string but the symbol
(defun replique/thing-at-point ()
  (let ((s-pps (syntax-ppss)))
    (if (not (null (nth 3 s-pps)))
        (save-excursion
          (goto-char (nth 8 s-pps))
          (thing-at-point 'sexp))
      (thing-at-point 'sexp))))

(defun replique/bounds-of-thing-at-point ()
  (let ((s-pps (syntax-ppss)))
    (if (not (null (nth 3 s-pps)))
        (save-excursion
          (goto-char (nth 8 s-pps))
          (bounds-of-thing-at-point 'sexp))
      (bounds-of-thing-at-point 'sexp))))

(defun replique/unwrap-comment (prev-bounds)
  (let ((sexpr-start (nth 1 (syntax-ppss))))
    (if (null sexpr-start)
        (let ((prev-expr (when prev-bounds
                           (buffer-substring-no-properties
                            (car prev-bounds) (cdr prev-bounds)))))
          (if (or (null prev-expr) (seq-contains replique/clj-comment prev-expr))
              (replique/bounds-of-thing-at-point)
            (if (seq-find (lambda (comment-expr)
                            (let* ((comment-expr (concat "(" comment-expr))
                                   (bound (+ (point) (length comment-expr))))
                              (search-forward comment-expr bound t)))
                          replique/clj-comment)
                prev-bounds
              (replique/bounds-of-thing-at-point))))
      (let ((bounds (replique/bounds-of-thing-at-point)))
        (goto-char sexpr-start)
        (replique/unwrap-comment bounds)))))

(defun replique/eval-defn (p)
  "Eval the top level sexpr at point"
  (interactive "P")
  (save-excursion
    (let* ((expr-bounds (replique/unwrap-comment nil))
           (expr (when expr-bounds
                   (buffer-substring-no-properties (car expr-bounds) (cdr expr-bounds)))))
      (when expr
        (replique/maybe-change-ns)
        (let* ((line (replique/line-number-at-pos (car expr-bounds)))
               (column (1+ (replique/column-number-at-pos (car expr-bounds))))
               (replique/eval-from-source-meta (replique/hash-map
                                                :line line
                                                :column column
                                                :url (replique/buffer-url
                                                      (buffer-file-name)))))
          (replique/send-input-from-source-dispatch
           (if p
               (concat "(replique.omniscient/with-redefs " expr ")")
             expr)))))))

(defun replique/load-url-clj (url p props clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/send-input-from-source-clj
     (format (if p
                 "(replique.omniscient/with-redefs (replique.interactive/load-url \"%s\"))"
               "(replique.interactive/load-url \"%s\")")
             url)
     props clj-repl)))

(defun replique/load-url-cljs (url p props cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/send-input-from-source-cljs
     (format (if p
                 "(replique.omniscient/with-redefs (replique.interactive/load-url \"%s\"))"
               "(replique.interactive/load-url \"%s\")")
             url)
     props cljs-repl)))

(defun replique/load-url-cljc (url p props repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique/send-input-from-source-cljc
     (format (if p
                 "(replique.omniscient/with-redefs (replique.interactive/load-url \"%s\"))"
               "(replique.interactive/load-url \"%s\")")
             url)
     props repl)))

(defun replique/buffer-url (file-name)
  (if (string-match "^\\(.*\\.jar\\):\\(.+\\)" file-name)
      (format "jar:file:%s!/%s" (match-string 1 file-name) (match-string 2 file-name))
    (format "file://%s" file-name)))

(defun replique/load-file (file-path p)
  (interactive (list (buffer-file-name) current-prefix-arg))
  (comint-check-source file-path)
  (replique/with-modes-dispatch
   (clojure-mode . (apply-partially 'replique/load-url-clj
                                    (replique/buffer-url file-path) p))
   (clojurescript-mode . (apply-partially 'replique/load-url-cljs
                                          (replique/buffer-url file-path) p))
   (clojurec-mode . (apply-partially 'replique/load-url-cljc
                                     (replique/buffer-url file-path) p))
   (css-mode . (apply-partially 'replique/load-css file-path))
   (js2-mode . (apply-partially 'replique/load-js file-path))
   (stylus-mode . (apply-partially 'replique/load-stylus file-path))
   (scss-mode . (apply-partially 'replique/load-scss file-path))
   (sass-mode . (apply-partially 'replique/load-sass file-path))
   (less-css-mode . (apply-partially 'replique/load-less file-path))))

(defun replique/reload-all-clj (props clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/send-input-from-source-clj
     (format "(require '%s :reload-all)" (replique-context/clojure-find-ns))
     props clj-repl)))

(defun replique/reload-all-cljs (props cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/send-input-from-source-cljs
     (format "(replique.interactive/load-url \"%s\" :reload-all)"
             (replique/buffer-url (buffer-file-name)))
     props cljs-repl)))

(defun replique/reload-all-cljc (props repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (let ((form (if (equal :cljs (replique/get repl :repl-type))
                    (format "(replique.interactive/load-url \"%s\" :reload-all)"
                            (replique/buffer-url (buffer-file-name)))
                  (format "(require '%s :reload-all)" (replique-context/clojure-find-ns)))))
      (replique/send-input-from-source-cljc form props repl))))

(defun replique/reload-all ()
  (interactive)
  (comint-check-source (buffer-file-name))
  (replique/with-modes-dispatch
   (clojure-mode . 'replique/reload-all-clj)
   (clojurescript-mode . 'replique/reload-all-cljs)
   (clojurec-mode . 'replique/reload-all-cljc)))

(defun replique/browser ()
  "Open a browser tab on the port the REPL is listening to"
  (interactive)
  (let ((cljs-repl (replique/active-repl :cljs)))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (browse-url (format "http://localhost:%s" (replique/get cljs-repl :port)))))

(defun replique/switch-active-repl (repl-buff-name)
  "Switch the currently active REPL session"
  (interactive
   (let* ((active-repl (replique/active-repl :tooling t))
          (directory (replique/get active-repl :directory))
          (repls (seq-filter (lambda (repl)
                               (let ((repl-type (replique/get repl :repl-type))
                                     (d (replique/get repl :directory)))
                                 (and
                                  (equal d directory)
                                  (not (equal :tooling repl-type)))))
                             replique/repls))
          (repl-names (mapcar (lambda (repl)
                                (buffer-name (replique/get repl :buffer)))
                              repls)))
     (when (null repls)
       (user-error "No started REPL"))
     (list (completing-read "Switch to REPL: " repl-names nil t))))
  ;; All windows displaying the previously active repl are set to display the newly active
  ;; repl (repl with the same repl-type)
  (let* ((buffer (get-buffer repl-buff-name))
         (repl (replique/repl-by :buffer buffer))
         (repl-type (replique/get repl :repl-type))
         (prev-active-repl (replique/repl-by :repl-type repl-type))
         (prev-active-buffer (replique/get prev-active-repl :buffer))
         (prev-active-windows (get-buffer-window-list prev-active-buffer)))
    (setq replique/repls (delete repl replique/repls))
    (setq replique/repls (push repl replique/repls))
    (mapcar (lambda (window)
              (set-window-buffer window buffer))
            prev-active-windows)))

(defun replique/switch-active-process (proc-name)
  "Switch the currently active JVM process"
  (interactive
   (let* ((tooling-repls (replique/repls-by :repl-type :tooling))
          (directories (mapcar (lambda (repl) (replique/get repl :directory))
                               tooling-repls)))
     (when (not (car directories))
       (user-error "No started REPL"))
     (list (completing-read "Switch to process: " directories nil t))))
  (let* ((new-active-repls (replique/repls-by :directory proc-name))
         (prev-active-tooling-repl (replique/active-repl :tooling))
         (prev-active-directory (replique/get prev-active-tooling-repl :directory))
         (prev-repls (replique/repls-by :directory prev-active-directory))
         (prev-repl-types (mapcar (lambda (repl) (replique/get repl :repl-type)) prev-repls)))
    ;; All windows displaying the previously active repls are set to display the newly active
    ;; repls (repl with the same repl-type and same position in the replique/repls list)
    (dolist (repl-type prev-repl-types)
      (let* ((prev-active-repl (replique/repl-by :directory prev-active-directory
                                                 :repl-type repl-type))
             (prev-buffer (replique/get prev-active-repl :buffer))
             (new-active-repl (replique/repl-by
                               :directory proc-name
                               :repl-type repl-type))
             (new-buffer (replique/get new-active-repl :buffer))
             (windows (get-buffer-window-list prev-buffer)))
        (when new-buffer
          (dolist (window windows)
            (set-window-buffer window new-buffer)))))
    (dolist (repl (seq-reverse new-active-repls))
      (setq replique/repls (delete repl replique/repls))
      (setq replique/repls (push repl replique/repls)))))

(defun replique/list-cljs-namespaces (tooling-repl)
  (let ((resp (replique/send-tooling-msg
               tooling-repl
               (replique/hash-map :type :list-namespaces
                                  :repl-env :replique/cljs))))
    (let ((err (replique/get resp :error)))
      (when err
        (message "%s" (replique-edn/pr-str err))
        (message "list-namespaces failed")))
    resp))

(defun replique/cljs-repl ()
  "Start a Clojurescript REPL in the currently active Clojure REPL"
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (clj-repl (replique/active-repl :clj)))
    (when (not clj-repl)
      (user-error "No active Clojure REPL"))
    (replique/send-input-from-source-clj
     "(replique.interactive/cljs-repl)" tooling-repl clj-repl)))

(defun replique/cljs-repl-nashorn ()
  "Start a Clojurescript Nashorn REPL in the currently active Clojure REPL"
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (clj-repl (replique/active-repl :clj)))
    (when (not clj-repl)
      (user-error "No active Clojure REPL"))
    (replique/send-input-from-source-clj
     "(replique.interactive/cljs-repl-nashorn)" tooling-repl clj-repl)))

(defun replique/output-main-js-file (output-to &optional main-ns)
  "Write a main javascript file to disk. The main javascript file acts as an entry point to your application and contains code to connect to the Clojurescript REPL"
  (interactive (list nil))
  (let ((tooling-repl (replique/active-repl :tooling t)))
    (when-let ((output-to (or
                           output-to
                           (read-file-name "Output main js file to: "
                                           (replique/get tooling-repl :directory)))))
      (let ((output-to (file-truename output-to)))
        (when (file-directory-p output-to)
          (user-error "%s is a directory" output-to))
        (when (or (not (file-exists-p output-to))
                  (yes-or-no-p (format "Override %s?" output-to)))
          (let ((resp (if main-ns
                          (replique/hash-map :namespaces nil)
                        (replique/list-cljs-namespaces tooling-repl))))
            (when (not (replique/get resp :error))
              (let* ((namespaces (replique/get resp :namespaces))
                     (main-ns (cond (main-ns main-ns)
                                    ((seq-empty-p namespaces) nil)
                                    (t (replique/return-nil-on-quit
                                        (completing-read "Main Clojurescript namespace: "
                                                         namespaces nil t nil nil '(nil))))))
                     (resp (replique/send-tooling-msg
                            tooling-repl
                            (replique/hash-map :type :output-main-js-files
                                               :repl-env :replique/cljs
                                               :output-to output-to
                                               :main-ns main-ns))))
                (let ((err (replique/get resp :error)))
                  (if err
                      (progn
                        (message "%s" (replique-edn/pr-str err))
                        (message "output-main-js-file failed"))
                    (message "Main javascript file written to: %s" output-to)))))))))))

;; I don't know why goog/deps.js is needed here but is not needed in the init script returned
;; by the cljs repl
;; Wait a second before connecting to the cljs repl because there is no way to plug into
;; the goog.require loading mechanism
(defun replique/cljs-repl-connection-script ()
  (interactive)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (port (replique/get tooling-repl :port))
         (script (format "(function() {
  var scripts = [];
  var makeScript = function() {
    var script = document.createElement(\"script\");
    scripts.push(script);
    return script;
  }
  var execScripts = function() {
    if(scripts.length > 0) {
      scripts[0].onload = execScripts;
      document.body.appendChild(scripts.shift());
    }
  }
  makeScript().src = \"http://localhost:%s/goog/base.js\";
  makeScript().src = \"http://localhost:%s/goog/deps.js\";
  makeScript().src = \"http://localhost:%s/cljs_deps.js\";
  makeScript().src = \"http://localhost:%s/replique/cljs_env/bootstrap.js\";
  makeScript().textContent = \"goog.require(\\\"replique.cljs_env.repl\\\"); goog.require(\\\"replique.cljs_env.browser\\\");\";
  execScripts();
  setTimeout(function(){replique.cljs_env.repl.connect(\"http://localhost:%s\");}, 1000);
})();" port port port port port)))
    (kill-new script)
    (message script)))

(defun replique/list-css (tooling-repl cljs-repl)
  (let* ((repl-env (replique/get cljs-repl :repl-env))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :list-css
                                   :repl-env repl-env))))
    (let ((err (replique/get resp :error)))
      (when err
        (message "%s" (replique-edn/pr-str err))
        (message "list-css failed")))
    resp))

(defun replique/asset-url-matches? (file-name url)
  (let ((url-filename (file-name-nondirectory (url-filename (url-generic-parse-url url)))))
    (equal file-name url-filename)))

(defvar replique/css-prefered-files (replique/hash-map))

;; Choose a css url either by looking into replique/css-prefered-files (if the user already
;; has chosen one before) or by prompting the user
(defun replique/select-css-url (file-path css-urls)
  (or (replique/get replique/css-prefered-files file-path)
      (let ((selected-url (completing-read "Select the css file to reload: " css-urls nil t)))
        (setq replique/css-prefered-files (replique/assoc replique/css-prefered-files
                                                          file-path selected-url))
        selected-url)))

(defun replique/load-css (file-path tooling-repl cljs-repl)
  (when (not cljs-repl)
    (user-error "No active Clojurescript REPL"))
  (let* ((repl-env (replique/get cljs-repl :repl-env))
         (resp (replique/list-css tooling-repl cljs-repl)))
    (when (not (replique/get resp :error))
      (let* ((css-urls (replique/get resp :css-urls))
             (css-urls (seq-filter (apply-partially 'replique/asset-url-matches?
                                                    (file-name-nondirectory file-path))
                                   css-urls))
             (selected-url (cond ((equal 0 (length css-urls))
                                  ;; No stylesheet matches, do nothing
                                  nil)
                                 ((equal 1 (length css-urls))
                                  ;; Reload the stylesheet that matches
                                  (car css-urls))
                                 (t (replique/select-css-url file-path css-urls)))))
        (if (null selected-url)
            (message "Could not find a css file to reload")
          (let ((resp (replique/send-tooling-msg
                       tooling-repl
                       (replique/hash-map :type :load-css
                                          :repl-env repl-env
                                          :url selected-url))))
            (let ((err (replique/get resp :error)))
              (if err
                  (progn
                    (message "%s" (replique-edn/pr-str err))
                    (message "load-css %s: failed" file-path))
                (message "load-css %s: done" file-path)))))))))

(defun replique/load-js (file-path tooling-repl cljs-repl)
  (when (not cljs-repl)
    (user-error "No active Clojurescript REPL"))
  (let* ((repl-env (replique/get cljs-repl :repl-env))
         (resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :load-js
                                   :repl-env repl-env
                                   :file-path file-path))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-edn/pr-str err))
            (message "load-js %s: failed" file-path))
        (message "load-js %s: done" file-path)))))

;; {directory {css-preprocessor-type {:output-files {output-file main-source-file}
;;                                   {:last-selected-output-file output-file}}}}
;; When the user triggers a compilation, he is prompted for an output css file. The
;; compilation is performed using the main source file.
(defvar replique/css-preprocessors-output-files (replique/hash-map))

;; Move x to the first position in coll
(defun replique/move-to-front (coll x)
  (when (and coll (seq-contains coll x))
    (thread-last coll
      (seq-remove (lambda (y) (equal x y)))
      (cons x))))

(defcustom replique/stylus-executable "stylus"
  "Stylus executable path"
  :type 'string
  :group 'replique)

(defun replique/stylus-args-builder-default (input output)
  (list "-m" "--sourcemap-inline" "--out" output input))

(defvar replique/stylus-args-builder 'replique/stylus-args-builder-default
  "Function that returns the command to use when compiling stylus files")

(defcustom replique/sass-executable "sass"
  "Sass executable path"
  :type 'string
  :group 'replique)

(defun replique/sass-args-builder-default (input output)
  (list "--sourcemap=inline" "--default-encoding" "utf-8" input output))

(defvar replique/sass-args-builder 'replique/sass-args-builder-default
  "Function that returns the command to use when compiling sass files")

(defcustom replique/scss-executable "scss"
  "Scss executable path"
  :type 'string
  :group 'replique)

(defun replique/scss-args-builder-default (input output)
  (list "--sourcemap=inline" "--default-encoding" "utf-8" input output))

(defvar replique/scss-args-builder 'replique/scss-args-builder-default
  "Function that returns the command to use when compiling scss files")

(defcustom replique/less-executable "lessc"
  "Scss executable path"
  :type 'string
  :group 'replique)

(defun replique/less-args-builder-default (input output)
  (list "--source-map" "--source-map-less-inline" input output))

(defvar replique/less-args-builder 'replique/less-args-builder-default
  "Function that returns the command to use when compiling less files")

(defun replique/load-css-preprocessor
    (type executable args-builder file-path tooling-repl cljs-repl)
  (when (not cljs-repl)
    (user-error "No active Clojurescript REPL"))
  (let* ((directory (replique/get tooling-repl :directory))
         (output-files (replique/get-in replique/css-preprocessors-output-files
                                        `[,directory ,type :output-files]))
         (output-files (replique/keys output-files))
         (last-selected-output-file (replique/get-in
                                     replique/css-preprocessors-output-files
                                     `[,directory ,type :last-selected-output-file]))
         (output-files (replique/move-to-front output-files last-selected-output-file))
         ;; We don't want ivy to sort candidates by alphabetical order
         (ivy-sort-functions-alist nil)
         (output (if output-files
                     (completing-read "Compile to file: "
                                      ;; All candidates are lists in order for completing-read
                                      ;; to keep the order of the candidates
                                      (replique/presorted-completion-table
                                       (append output-files '("*new-file*")))
                                      nil t)
                   "*new-file*"))
         (is-new-file (equal output "*new-file*"))
         (output (if is-new-file
                     (read-file-name "Compile to file: "
                                     (replique/get tooling-repl :directory)
                                     nil t)
                   output))
         (main-source-file (if is-new-file
                               file-path
                             (replique/get-in replique/css-preprocessors-output-files
                                              `[,directory ,type :output-files ,output])))
         (output (file-truename output))
         (command-result 0))
    (when (file-directory-p output)
      (user-error "%s is a directory" output))
    (when (or (not is-new-file)
              (not (file-exists-p output))
              (yes-or-no-p (format "Override %s?" output)))
      (with-temp-buffer
        (setq command-result (apply 'call-process executable nil t nil
                                    (funcall args-builder main-source-file output)))
        (when (not (equal 0 command-result))
          (ansi-color-apply-on-region (point-min) (point-max))
          (message (buffer-substring (point-min) (point-max)))))
      (when (equal 0 command-result)
        (setq replique/css-preprocessors-output-files
              (replique/update-in
               replique/css-preprocessors-output-files
               `[,directory ,type]
               (lambda (x)
                 (thread-first x
                   (replique/assoc :last-selected-output-file output)
                   (replique/assoc-in `[:output-files ,output] main-source-file)))))
        (replique/load-css output tooling-repl cljs-repl)))))

(defun replique/load-stylus (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :stylus replique/stylus-executable
                                  replique/stylus-args-builder
                                  file-path tooling-repl cljs-repl))


(defun replique/load-scss (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :scss replique/scss-executable
                                  replique/scss-args-builder
                                  file-path tooling-repl cljs-repl))

(defun replique/load-sass (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :sass replique/sass-executable
                                  replique/sass-args-builder
                                  file-path tooling-repl cljs-repl))

(defun replique/load-less (file-path tooling-repl cljs-repl)
  (replique/load-css-preprocessor :less replique/less-executable
                                  replique/less-args-builder
                                  file-path tooling-repl cljs-repl))

(defun replique/eval-form (repl-type form-s)
  (let ((repl (replique/active-repl repl-type)))
    (if (null repl)
        (format "No active %s REPL" repl-type)
      (let* ((form-s (replace-regexp-in-string "\n" " " form-s))
             (tooling-repl (replique/active-repl :tooling t))
             (repl-env (replique/get repl :repl-env))
             (resp (replique/send-tooling-msg
                    tooling-repl
                    (replique/hash-map :type :eval
                                       :repl-env repl-env
                                       :form form-s))))
        (let ((err (replique/get resp :error)))
          (if err
              (replique-edn/pr-str err)
            (replique/get resp :result)))))))
(comment
 (replique/eval-form :clj "(+ 1 4)")
 )

(defun replique/jump-to-definition* (symbol tooling-repl repl-env ns)
  (let* ((resp (replique/send-tooling-msg
                tooling-repl
                (replique/hash-map :type :meta
                                   :repl-env repl-env
                                   :ns ns
                                   :symbol symbol
                                   :context (replique-context/get-context repl-env)))))
    (let ((err (replique/get resp :error)))
      (if err
          (progn
            (message "%s" (replique-edn/pr-str err))
            (message "jump-to-definition failed with symbol: %s" symbol))
        (let* ((meta (replique/get resp :meta))
               (file (replique/get-in resp [:meta :file]))
               (line (replique/get-in resp [:meta :line]))
               (column (replique/get-in resp [:meta :column])))
          (cond ((not (replique-transit/is-small-number? line))
                 (message "Cannot handle line number %s" line)
                 (message "jump-to-definition failed with symbol: %s" symbol))
                ((not (replique-transit/is-small-number? column))
                 (message "Cannot handle column number %s" column)
                 (message "jump-to-definition failed with symbol: %s" symbol))
                (t
                 (when-let (buff (replique-resources/find-file file))
                   (xref-push-marker-stack)
                   (pop-to-buffer-same-window buff)
                   (goto-char (point-min))
                   (when line
                     (forward-line (1- line))
                     (when column
                       (move-to-column column)))))))))))

(defun replique/jump-to-definition-session (symbol repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-env (replique/get repl :repl-env))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/jump-to-definition* symbol tooling-repl repl-env ns)))

(defun replique/jump-to-definition-clj (symbol tooling-repl clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/jump-to-definition*
     symbol tooling-repl (replique/get clj-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/jump-to-definition-cljs (symbol tooling-repl cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/jump-to-definition*
     symbol tooling-repl (replique/get cljs-repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/jump-to-definition-cljc (symbol tooling-repl repl)
  (if (not repl)
      (user-error "No active Clojure or Clojurescript REPL")
    (replique/jump-to-definition*
     symbol tooling-repl (replique/get repl :repl-env) (replique-context/clojure-find-ns))))

(defun replique/jump-to-definition (symbol)
  "Jump to symbol at point definition, if the metadata for the symbol at point contains enough information"
  (interactive (list (let ((sym (symbol-at-point)))
                       (when sym (symbol-name sym)))))
  (replique/with-modes-dispatch
   (replique/mode . (apply-partially 'replique/jump-to-definition-session symbol))
   (clojure-mode . (apply-partially 'replique/jump-to-definition-clj symbol))
   (clojurescript-mode . (apply-partially 'replique/jump-to-definition-cljs symbol))
   (clojurec-mode . (apply-partially 'replique/jump-to-definition-cljc symbol))
   (t . (user-error "Unsupported major mode: %s" major-mode))))

(defun replique/offline-mode ()
  "Toggle the Replique offline mode. When offline mode is enabled, leiningen is started index
offline mode"
  (interactive)
  (if replique/offline-mode-flag
      (progn
        (message "Offline mode disabled")
        (setq replique/offline-mode-flag nil))
    (message "Offline mode enabled")
    (setq replique/offline-mode-flag t)))

(comment
 (defun replique/install-node-deps ()
   "Install Clojurescript :npm-deps"
   (interactive)
   (let* ((tooling-repl (replique/active-repl :tooling t))
          (active-repl (replique/active-repl '(:clj :cljs))))
     (when (not active-repl)
       (user-error "No active Clojure REPL"))
     (replique/send-input-from-source-session "(replique.interactive/install-node-deps!)"
                                              active-repl)))
 )

(defconst replique/client-version "0.0.9-SNAPSHOT")

(defcustom replique/version "0.0.9-SNAPSHOT"
  "Hook for customizing the version of the replique REPL server to be used"
  :type 'string
  :group 'replique)

(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

(defcustom replique/prompt-read-only t
  "Whether to make the prompt read-only or not"
  :type 'boolean
  :group 'replique)

(defcustom replique/lein-script nil
  "Leiningen script path"
  :type 'file
  :group 'replique)

(defcustom replique/default-lein-script "lein"
  "Name of the lein script to be used when replique/lein-script is not set. The script will 
be searched in exec-path."
  :type 'string
  :group 'replique)

(defcustom replique/host "localhost"
  "Replique REPLs listening socket host"
  :type 'string
  :group 'replique)

(defcustom replique/main-js-files-max-depth 5
  "Maximum directory nesting when search for main js files. Set to a negative value to
disable main js files refreshing."
  :type 'integer
  :group 'replique)

(defcustom replique/company-tooltip-align-annotations t
  "See company-tooltip-align-annotations"
  :type 'boolean
  :group 'replique)

(defcustom replique/display-output-in-minibuffer t
  "Whether the output of the active REPL is displayed in the minibuffer when the REPL buffer 
is not visible"
  :type 'boolean
  :group 'replique)

(defface replique/minibuffer-output-repl-name-face
  '((t :slant italic :height 110 :overline t))
  "Face for displaying the name of the REPL the output being displayed came from"
  :group 'replique )

(defcustom replique/clj-comment '("comment" "clojure.core/comment")
  "The symbols considered to be starting a clojure comment block. Used by replique/eval-defn when
unwrapping a top level comment block "
  :type 'list
  :group 'replique)

(defcustom replique/offline-mode-flag nil
  "Whether to run leiningen in offline mode or not"
  :type 'boolean
  :group 'replique)

(defvar replique/mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" 'replique/comint-send-input)
    (define-key map "\C-c\C-r" 'replique/switch-active-repl)
    (define-key map "\M-." 'replique/jump-to-definition)
    (define-key map "\C-c\C-o" 'replique/omniscient)
    map))

(defun replique/commons ())

(define-derived-mode replique/mode comint-mode "Replique"
  "Commands:\\<replique/mode-map>"
  (setq-local comint-prompt-regexp replique/prompt)
  (setq-local comint-prompt-read-only replique/prompt-read-only)
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (set-syntax-table clojure-mode-syntax-table)
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'replique/company-backend)
    (setq-local company-tooltip-align-annotations
                replique/company-tooltip-align-annotations))
  (add-function :before-until (local 'eldoc-documentation-function)
                'replique/eldoc-documentation-function))

(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-r" 'replique/eval-region)
    (define-key map "\C-x\C-e" 'replique/eval-last-sexp)
    (define-key map "\C-\M-x" 'replique/eval-defn)
    (define-key map "\C-c\C-l" 'replique/load-file)
    (define-key map "\C-c\M-n" 'replique/in-ns)
    (define-key map "\C-c\C-r" 'replique/switch-active-repl)
    (define-key map "\M-." 'replique/jump-to-definition)
    (define-key map "\M-," 'xref-pop-marker-stack)
    (define-key map "\C-c\C-o" 'replique/omniscient)
    (easy-menu-define replique/minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique/eval-region t]
        ["Eval last sexp" replique/eval-last-sexp t]
        ["Eval defn" replique/eval-defn t]
        "--"
        ["Load file" replique/load-file t]
        "--"
        ["Set REPL ns" replique/in-ns t]
        "--"
        ["Switch active REPL" replique/switch-active-repl t]
        "--"
        ["Jump to definition" replique/jump-to-definition t]
        "--"
        ["Omniscient action" replique/omniscient t]
        ))
    map))

;;;###autoload
(define-minor-mode replique/minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/minor-mode-map}"
  :lighter "Replique" :keymap replique/minor-mode-map
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'replique/company-backend)
    (setq-local company-tooltip-align-annotations
                replique/company-tooltip-align-annotations))
  (add-function :before-until (local 'eldoc-documentation-function)
                'replique/eldoc-documentation-function))

;; lein update-in :plugins conj "[replique/replique \"0.0.1-SNAPSHOT\"]" -- trampoline replique localhost 9000

(defun replique/lein-command (host port directory)
  `(,(or replique/lein-script (executable-find replique/default-lein-script))
    ,@(when replique/offline-mode-flag '("-o"))
    "update-in" ":plugins" "conj"
    ,(format "[replique/replique \"%s\"]" replique/version)
    "--"
    "trampoline" "replique"
    ,(format "%s" host) ,(format "%s" port)
    ,(format "%s" directory)))

(defun replique/is-valid-port-nb? (port-nb)
  (< -1 port-nb 65535))

(defvar replique/correlation-id 0)
(defvar replique/synchronous-messages (replique/hash-map))

(defun replique/send-tooling-msg (tooling-repl msg)
  (let* ((tooling-network-proc (replique/get tooling-repl :network-proc))
         (process-id (replique/get tooling-repl :directory))
         (correlation-id replique/correlation-id)
         (msg (replique/assoc msg
                              :process-id process-id
                              :correlation-id correlation-id)))
    (puthash correlation-id replique/nothing replique/synchronous-messages)
    (setq replique/correlation-id  (if (= most-positive-fixnum correlation-id)
                                       0
                                     (+ 1 correlation-id)))
    (process-send-string
     tooling-network-proc
     (format "(replique.tooling-msg/tooling-msg-handle %s)\n" (replique-edn/pr-str msg)))
    ;; Synchronously wait for a response from the tooling REPL.
    ;; accept-process-output maybe be wake up by a quit signal or some output from the network
    ;; process. The network process output may be split into multiple incomplete messages chunks.
    ;; Also, multiple call to replique/send-tooling-msg may happen simultaneously because of timers
    ;; and the output received from the network process may not wake up all blocking calls to
    ;; accept-process-output
    (let ((inhibit-quit t)
          (wait-output? t)
          (result nil))
      (while wait-output?
        (with-local-quit (accept-process-output tooling-network-proc nil nil t))
        (let ((sync-msg (replique/get replique/synchronous-messages correlation-id)))
          (cond ((not (eq replique/nothing sync-msg))
                 (remhash correlation-id replique/synchronous-messages)
                 ;; There may be other blocking calls to accept-process-output. Try to wake up them
                 ;; by sending a block message
                 (process-send-string tooling-network-proc "nil\n")
                 (setq quit-flag nil)
                 (setq wait-output? nil)
                 (setq result sync-msg))
                (quit-flag (remhash correlation-id replique/synchronous-messages)))))
      (replique-transit/decode result))))

(defun replique/close-tooling-repl (tooling-repl)
  (let ((tooling-proc (replique/get tooling-repl :proc)))
    (when (process-live-p tooling-proc)
      ;; SIGINT, the jvm should stop, even if there is pending non daemon threads, shutdown
      ;; hooks should run
      (interrupt-process tooling-proc))
    (setq replique/repls (delete tooling-repl replique/repls))))

(defun replique/maybe-close-tooling-repl (host port)
  (let ((other-repls (replique/repls-by :host host :port port)))
    (when (seq-every-p (lambda (repl)
                         (equal :tooling (replique/get repl :repl-type)))
                       other-repls)
      (mapcar (lambda (tooling-repl)
                (replique/close-tooling-repl tooling-repl))
              other-repls))))

(defun replique/close-repl (repl kill-buffers)
  (let* ((buffer (replique/get repl :buffer))
         (host (replique/get repl :host))
         (port (replique/get repl :port))
         (proc (get-buffer-process buffer)))
    (when proc
      (set-process-sentinel proc nil)
      (delete-process proc))
    (if kill-buffers
        (kill-buffer buffer)
      (with-current-buffer buffer
        ;; Disable replique/mode
        (fundamental-mode)
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\nConnection broken by remote peer\n")))))
    (setq replique/repls (delete repl replique/repls))
    ;; If the only repl left is a tooling repl, let's close it
    (replique/maybe-close-tooling-repl host port)))

(defun replique/close-repls (host port kill-buffers)
  (let* ((repls (replique/repls-by :host host :port port))
         (not-tooling-repls (seq-filter (lambda (repl)
                                          (not (equal :tooling (replique/get repl :repl-type))))
                                        repls))
         (tooling-repls (seq-filter (lambda (repl)
                                      (equal :tooling (replique/get repl :repl-type)))
                                    repls)))
    (dolist (repl not-tooling-repls)
      (replique/close-repl repl kill-buffers))
    (dolist (tooling-repl tooling-repls)
      (replique/close-tooling-repl tooling-repl))))

(defun replique/on-repl-close (buffer process event)
  (let ((closing-repl (replique/repl-by :buffer buffer)))
    (when closing-repl
      (cond ((string= "deleted\n" event)
             (replique/close-repl closing-repl t))
            ((string= "connection broken by remote peer\n" event)
             (replique/close-repl closing-repl nil))
            (t nil)))))

(defun replique/on-tooling-repl-close (network-proc-buffer host port process event)
  (cond ((string= "deleted\n" event)
         (replique/close-repls host port t))
        ((string= "connection broken by remote peer\n" event)
         (replique/close-repls host port nil))
        (t nil))
  (replique/kill-process-buffer network-proc-buffer))

(defun replique/check-lein-script ()
  (cond ((and replique/lein-script (not (executable-find replique/lein-script)))
         (format "Error while starting the REPL. %s could not be find in exec-path. Please update replique/lein-script" replique/lein-script))
        ((and (not replique/lein-script) (not (executable-find replique/default-lein-script)))
         "Error while starting the REPL. No lein script found in exec-path")
        (t nil)))

(defun replique/process-filter-skip-repl-starting-output (proc callback)
  (let ((started-regexp (format "Replique version %s listening on port \\([0-9]+\\)\n"
                                replique/version)))
    (set-process-filter
     proc (lambda (proc string)
            ;; REPL is started
            (if (string-match started-regexp string)
                (thread-last (match-string 1 string)
                  string-to-number
                  (funcall callback))
              ;; REPL is still starting. Print all the init messages
              (message "%s" string))))))

(defun replique/is-main-js-file (f)
  (let* ((first-line "//main-js-file autogenerated by replique")
         (check-length (length first-line)))
    (and (file-exists-p f)
         (condition-case err
             (with-temp-buffer
               (insert-file-contents f nil 0 check-length)
               (equal (buffer-substring-no-properties 1 (+ 1 check-length)) first-line))
           (error nil)))))

(defun replique/directory-files-recursively (dir regexp exclude-dir max-depth)
  (let ((exclude-dir (expand-file-name exclude-dir dir))
        (result nil)
        (files nil))
    (unless (< max-depth 0)
      (dolist (file (file-name-all-completions "" dir))
        (unless (member file '("./" "../"))
          (if (directory-name-p file)
              (let* ((leaf (substring file 0 (1- (length file))))
                     (full-file (expand-file-name leaf dir)))
                ;; Don't follow symlinks to other directories.
                (unless (or (file-symlink-p full-file) (equal full-file exclude-dir))
                  (setq result
                        (nconc result (replique/directory-files-recursively
                                       full-file regexp exclude-dir (1- max-depth))))))
            (when (string-match-p regexp file)
              (push (expand-file-name file dir) files))))))
    (nconc result files)))

(defun replique/refresh-main-js-files (directory port cljs-compile-path)
  (message "Refreshing main js files ...")
  (let* ((js-files (replique/directory-files-recursively
                    directory "\\.js$" cljs-compile-path replique/main-js-files-max-depth))
         (main-js-files (seq-filter 'replique/is-main-js-file js-files)))
    (dolist (f main-js-files)
      (with-temp-file f
        (insert-file-contents f)
        (save-match-data
          (when (re-search-forward "var port = \'[0-9]+\';" nil t)
            (replace-match (format "var port = \'%s\';" port))))
        (buffer-substring-no-properties 1 (point-max))))
    (message "Refreshing main js files ... %s files refreshed" (length main-js-files))))

;; We do not use (message ...) because messages may be received splitted and the message fn
;; always prints a new line. Instead we set a text property on the last char printed in the
;; messages buffer in order to detect two messages coming from the same process. When necessary,
;; messages are concatenated by direclty inserting them in the messages buffer
(defun replique/proc-message-fn (proc string)
  (when (not (eq "" string))
    (with-current-buffer (messages-buffer)
      (let ((inhibit-read-only t)
            (pname (process-name proc)))
        (save-excursion
          (goto-char (point-max))
          (if (and (> (point) 1)
                   (equal (get-text-property (1- (point)) 'replique/proc-out) pname))
              (insert string)
            (insert (format "\nProcess %s:\n%s" pname string)))
          (add-text-properties (1- (point)) (point) `(replique/proc-out
                                                      ,pname rear-nonsticky ,t)))))
    (replique/message-nolog
     (format
      "%s\n%s" string
      (propertize (process-name proc) 'face 'replique/minibuffer-output-repl-name-face)))))

(defun replique/make-tooling-repl (host port directory callback)
  (let* ((default-directory directory)
         (repl-cmd (replique/lein-command host port directory))
         (proc (apply 'start-process directory nil (car repl-cmd) (cdr repl-cmd))))
    (replique/process-filter-skip-repl-starting-output
     proc
     (lambda (port)
       ;; Print process messages
       (set-process-filter proc 'replique/proc-message-fn)
       (let* ((network-proc-buff (generate-new-buffer (format " *%s*" directory)))
              (network-proc (open-network-stream directory nil host port))
              (timeout (run-at-time
                        2 nil (lambda (proc)
                                (message "Error while starting the REPL. The port number may already be used by another process")
                                (when (process-live-p proc)
                                  (interrupt-process proc)))
                        proc)))
         (set-process-sentinel
          network-proc
          (apply-partially
           'replique/on-tooling-repl-close network-proc-buff host port))
         (replique/process-filter-read
          network-proc network-proc-buff
          (lambda (cljs-compile-path)
            (cancel-timer timeout)
            (let* ((cljs-compile-path (replique-transit/decode cljs-compile-path))
                   (tooling-repl (replique/hash-map
                                  :directory directory
                                  :repl-type :tooling
                                  :proc proc
                                  :network-proc network-proc
                                  :host host
                                  :port port)))
              (replique/process-filter-read
               network-proc network-proc-buff 'replique/dispatch-tooling-msg)
              (push tooling-repl replique/repls)
              (when (>= replique/main-js-files-max-depth 0)
                (replique/refresh-main-js-files directory port cljs-compile-path))
              (funcall callback tooling-repl))))
         ;; The REPL accept fn will read a char in order to check whether the request
         ;; is an HTTP one or not
         (process-send-string network-proc "R")
         ;; No need to wait for the return value of shared-tooling-repl
         ;; since it does not print anything
         (process-send-string
          network-proc "(replique.repl/shared-tooling-repl :elisp)\n")
         (process-send-string network-proc "replique.utils/cljs-compile-path\n"))))))

(defun replique/close-process (directory)
  "Close all the REPL sessions associated with a JVM process"
  (interactive
   (let* ((tooling-repls (replique/repls-by :repl-type :tooling :error-on-nil t))
          (directories (mapcar (lambda (repl) (replique/get repl :directory)) tooling-repls)))
     (list (completing-read "Close process: " directories nil t))))
  (let* ((tooling-repl (replique/repl-by :repl-type :tooling :directory directory))
         (host (replique/get tooling-repl :host))
         (port (replique/get tooling-repl :port)))
    (replique/close-repls host port t)))

(defun replique/time-diff (t1 t2)
  (float-time (time-subtract t1 t2)))

(defun replique/is-recent-output (now output)
  (let ((diff (replique/time-diff now (car output))))
    (< (float-time diff) 1)))

;;seq-take-while
(defun replique/concat-recent-output (recent-output)
  (let* ((recent-elements (thread-last (ring-elements recent-output)
                            (seq-take-while (apply-partially 'replique/is-recent-output
                                                             (current-time)))
                            (mapcar 'cadr)
                            reverse))
         (recent-elements (mapcar 'ansi-color-filter-apply recent-elements)))
    (apply 'concat recent-elements)))

(defun replique/comint-output-filter (process string)
  "Wrap comint-output-filter in order to display the output of hidden active REPLs in the 
minibuffer"
  (let* ((buffer (process-buffer process))
         (repl (replique/repl-by :buffer buffer))
         (recent-output (replique/get repl :recent-output))
         (active-repl-clj (replique/active-repl :clj))
         (active-repl-cljs (replique/active-repl :cljs)))
    (ring-insert recent-output (list (current-time) string))
    (when (and (or (eq active-repl-clj repl)
                   (eq active-repl-cljs repl))
               ;; do not print when a minibuffer is active
               (null (active-minibuffer-window))
               (not (seq-contains (replique/visible-buffers) buffer))
               replique/display-output-in-minibuffer)
      (replique/message-nolog
       (format "%s\n%s"
               (replique/concat-recent-output recent-output)
               (propertize (buffer-name buffer)
                           'face 'replique/minibuffer-output-repl-name-face))))
    (comint-output-filter process string)))

(defun replique/make-comint (proc buffer)
  (with-current-buffer buffer
    (unless (derived-mode-p 'comint-mode)
      (comint-mode))
    (set-process-filter proc 'replique/comint-output-filter)
    (setq-local comint-ptyp process-connection-type)
    ;; Jump to the end, and set the process mark.
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (run-hooks 'comint-exec-hook)
    buffer))

(defun replique/make-repl (buffer-name directory host port)
  (let* ((buff (generate-new-buffer buffer-name))
         (proc (open-network-stream buffer-name buff host port))
         (repl-cmd (format "(replique.repl/repl)\n")))
    (set-process-sentinel proc (apply-partially 'replique/on-repl-close buff))
    (replique/process-filter-read
     proc buff
     (lambda (resp)
       (let* ((resp (replique-transit/decode resp))
              (session (replique/get resp :client)))
         (set-process-filter proc nil)
         (replique/make-comint proc buff)
         (set-buffer buff)
         (replique/mode)
         (process-send-string proc repl-cmd)
         (let ((repl (replique/hash-map :directory directory
                                        :host host
                                        :port port
                                        :repl-type :clj
                                        :repl-env :replique/clj
                                        :session session
                                        :ns 'user
                                        :buffer buff
                                        :recent-output (make-ring 10))))
           (push repl replique/repls)
           (display-buffer buff)))))
    ;; The REPL accept fn will read a char in order to check whether the request
    ;; is an HTTP one or not
    (process-send-string proc "R")
    (process-send-string proc "replique.server/*session*\n")))

(defun replique/clj-buff-name (directory)
  (generate-new-buffer-name
   (format "*replique*%s*clj*" (file-name-nondirectory (directory-file-name directory)))))

(defun replique/normalize-directory-name (directory)
  (file-name-as-directory (expand-file-name directory)))

;;;###autoload
(defun replique/repl (directory &optional host port buffer-name)
  "Start a REPL session"
  (interactive
   (let ((directory (read-directory-name
                     "Project directory: " (replique/guess-project-root-dir) nil t)))
     ;; Normalizing the directory name is necessary in order to be able to find repls
     ;; by directory name
     (list (replique/normalize-directory-name directory))))
  (let* ((existing-repl (replique/repl-by :directory directory :repl-type :tooling))
         (host (cond (host host)
                     ((equal 4 (prefix-numeric-value current-prefix-arg))
                      (read-string "Hostname: "))
                     (t replique/host)))
         (port (cond
                (port port)
                (existing-repl
                 (replique/get existing-repl :port))
                (t (read-number "Port number: " 0))))
         (lein-script-error (replique/check-lein-script))
         (make-repl-fn (lambda (tooling-repl)
                         (let ((directory (replique/get tooling-repl :directory))
                               (port (replique/get tooling-repl :port))
                               (buffer-name (or buffer-name (replique/clj-buff-name directory))))
                           (condition-case err
                               (replique/make-repl buffer-name directory host port)
                             (error
                              (replique/maybe-close-tooling-repl host port)
                              ;; rethrow the error
                              (signal (car err) (cdr err))))))))
    (cond
     ((not (replique/is-valid-port-nb? port))
      (user-error "Invalid port number: %d" port))
     (lein-script-error
      (user-error lein-script-error))
     (t
      (if existing-repl
          (funcall make-repl-fn existing-repl)
        (replique/make-tooling-repl host port directory make-repl-fn))))))

(defun replique/on-repl-type-change (repl new-repl-type new-repl-env)
  (let* ((directory (replique/get repl :directory))
         (repl-type (replique/get repl :repl-type))
         (repl-type-s (replique/keyword-to-string repl-type))
         (repl-env (replique/get repl :repl-env))
         (new-repl-type-s (replique/keyword-to-string new-repl-type))
         (repl-buffer (replique/get repl :buffer)))
    (when (not (equal repl-type new-repl-type))
      (when (string-match-p (format "\\*%s\\*\\(<[0-9]+>\\)?$" repl-type-s)
                            (buffer-name repl-buffer))
        (let* ((new-buffer-name (replace-regexp-in-string
                                 (format "\\*%s\\*\\(<[0-9]+>\\)?$" repl-type-s)
                                 (format "*%s*" new-repl-type-s)
                                 (buffer-name repl-buffer))))
          (with-current-buffer repl-buffer
            (rename-buffer (generate-new-buffer-name new-buffer-name))))))
    (if (and (equal repl-type new-repl-type) (equal repl-env new-repl-env))
        repl
      (replique/update-repl repl (replique/assoc repl
                                                 :repl-type new-repl-type
                                                 :repl-env new-repl-env)))))

(defun replique/dispatch-async-msg (msg)
  (let ((msg (replique-transit/decode msg)))
    (cond
     (;; Global error (uncaught exception)
      (equal :error (replique/get msg :type))
      (message "%s - Thread: %s - Exception: %s"
               (propertize "Uncaught exception" 'face '(:foreground "red"))
               (replique/get msg :thread)
               (replique-edn/pr-str (ansi-color-filter-apply (replique/get msg :value)))))
     ((equal :repl-meta (replique/get msg :type))
      (let* ((repl (replique/repl-by
                    :session (replique/get (replique/get msg :session) :client)
                    :directory (replique/get msg :process-id))))
        (when repl
          (let ((repl (replique/on-repl-type-change
                       repl (replique/get msg :repl-type) (replique/get msg :repl-env))))
            (replique/update-repl repl (replique/assoc repl :ns (replique/get msg :ns)))))))
     (t (message "Received unsupported message while dispatching tooling messages: %s" msg)))))

(defun replique/with-new-dyn-context (fn &rest args)
  (apply 'run-at-time nil nil fn args))

(defun replique/dispatch-tooling-msg (msg)
  (cond ((null msg)
         ;; Ignore nil messages. This is only used to wake up (accept-process-output) blocking calls
         nil)
        ((not (hash-table-p msg))
         (message "Received invalid message while dispatching tooling messages: %s" msg))
        ((replique/get msg :correlation-id)
         (let* ((tooling-repl (replique/repl-by
                               :repl-type :tooling
                               :directory (replique/get msg :process-id)))
                (correlation-id (replique/get msg :correlation-id))
                (sync-msg (replique/get replique/synchronous-messages correlation-id)))
           (when sync-msg
             (puthash correlation-id msg replique/synchronous-messages))))
        (t (replique/with-new-dyn-context 'replique/dispatch-async-msg msg))))

(defvar replique/parser-state)

(defun replique/read-buffer* (dispatch-fn)
  (setq replique/parser-state (parse-partial-sexp
                               (point) (point-max) 0 nil replique/parser-state))
  (while (and
          ;; unbalanced expression or nothing to read
          (= 0 (car replique/parser-state))
          (not (null (nth 2 replique/parser-state))))
    (goto-char (point-min))
    (when-let ((o (read (current-buffer))))
      (funcall dispatch-fn o))
    (delete-region (point-min) (point))
    (setq replique/parser-state (parse-partial-sexp
                                 (point) (point-max) 0 nil replique/parser-state))))

(defun replique/read-buffer (string dispatch-fn proc buffer)
  (when (and string (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((p (point)))
        (insert string)
        (goto-char p))
      (replique/read-buffer* dispatch-fn))))

(defun replique/process-filter-read (proc buffer dispatch-fn)
  (with-current-buffer buffer
    (delete-region (point-min) (point-max))
    (setq replique/parser-state nil))
  (buffer-disable-undo buffer)
  (set-process-filter
   proc (lambda (proc string)
          (replique/read-buffer string dispatch-fn proc buffer))))

(defun replique/kill-process-buffer (buffer)
  (if (not (equal (buffer-size buffer) 0))
      ;; There is still stuff in the buffer, let a chance to the reader to finish reading the
      ;; process output
      (run-at-time 1 nil (lambda () (kill-buffer buffer)))
    (kill-buffer buffer)))

(provide 'replique)

;; compliment keywords cljs -> missing :require ... ?
;; CSS / HTML autocompletion, with core.spec ?
;; Customizing REPL options requires starting a new REPL (leiningen options don't work in the context of replique). Find a way to automate this process (using leiningen or not ...)
;; multi-process -> print directory in messages
;; The cljs-env makes no use of :repl-require

;; autocomplete using the spec first, compliment next if no candidates

;; spec autocomplete for files -> emacs first line local variables
;; spec autocomplete for macros -> specized macros

;; Document the use of cljsjs to use js libs
;; Customization var for excluded folders when refreshing main js files
;; completion for strings that match a path and are in a (File.) / (file) form
;; direct linking not supported because of alter-var-root! calls
;; Binding to the loopback address prevents connecting from the outside (mobile device ...)
;; cljs repl server hangs on serving assets on a broken connection ?
;; restore print-namespaced-maps somewhere
;; copy html / css on load-url (problem: override or not (web app context))
;; implement a replique lein profile (https://github.com/technomancy/leiningen/blob/master/doc/PLUGINS.md#evaluating-in-project-context)
;; add a compliment source for omniscient (:omniscient/quit for cljs + locals)
;; defmethod tooling-msg/tooling-msg-handle :eval-cljs -> opts are wrong (not enough things)
;; swap emacs buffers (if needed) when changing active repl from clj to cljs or cljs to clj (or not)
;; omniscient -> capture the stacktrace if possible
;; omniscient -> keep track of redefined vars, add the possibility to clear redefined vars
;; document omniscient global capture / rethink global capture for multithreads
;; jump-to-definition for ns -> list all files
;; new target directory for assets resources
;; elisp-printer -> escape symbols
;; defmethod locals (parameters) autocompletion
;; in-ns list namespaces
;; make-tooling-repl -> show buffer with starting progress
;; emacs 26 has built-in, faster "line-number-at-pos"
;; tooling-messages context -> parsing big contexts can be slow and reach the jvm method limit
;; check the infer-externs cljs option
;; comint-send-input -> check that depth is never negative (parse-partial-sexp)
;; eval interruption
;; invalidate classpath cache on classpath refresh

;; min versions -> clojure 1.8.0, clojurescript 1.9.473
;; byte-recompile to check warnings ----  M-x C-u 0 byte-recompile-directory

;; printing something that cannot be printed by the elisp printer results something that cannot be read by the reader because the elisp printer will print a partial object before throwing an exception

;; Several tooling message do not work when inside a verylarge form because the clojure compiler
;; reaches the method size limit - limit around 65000

(comment
 (local-set-key (kbd "C-c C-c") 'outline-hide-other)
 (local-set-key (kbd "C-s C-s") 'outline-show-all)
 )
