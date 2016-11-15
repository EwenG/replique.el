;;; replique2.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "25") (clojure-mode "4.0.1") (company "0.9.0") (s "1.9.0"))
;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'comint)
(require 'clojure-mode)
(require 'replique-edn)
(require 'replique-async)
(require 'company)
(require 'replique-hashmap)
(require 'map)

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
                              repl))
                      (repl (if (replique/contains? repl :chan)
                                (replique/assoc repl :chan '**)
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
                               replique/repls)))

(defun replique/repls-or-repl-by (filtering-fn source &rest args)
  (let* ((pred (lambda (repl)
                 (seq-every-p (lambda (arg)
                                (let ((k (car arg))
                                      (v (cdr arg)))
                                  (or (equal :error-on-nil k)
                                      (and (replique/contains? repl k)
                                           (equal v (replique/get repl k))))))
                              (replique/plist->alist args))))
         (found (funcall filtering-fn pred source)))
    (if (and (null found) (plist-get args :error-on-nil))
        (user-error "No started REPL")
      found)))

(defun replique/repl-by (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-find replique/repls args))

(defun replique/repls-by (&rest args)
  (apply 'replique/repls-or-repl-by 'seq-filter replique/repls args))

(defun replique/get-by (source &rest args)
  (apply 'replique/repls-or-repl-by 'seq-find source args))

(defun replique/get-all-by (source &rest args)
  (apply 'replique/repls-or-repl-by 'seq-filter source args))

(defun replique/active-repl (repl-type &optional error-on-nil)
  (if error-on-nil
      (replique/repl-by :repl-type repl-type :error-on-nil error-on-nil)
    (replique/repl-by :repl-type repl-type)))

(defun replique/guess-project-root-dir ()
  (or (locate-dominating-file default-directory "project.clj")
      default-directory))

(defun replique/replique-root-dir ()
  (thread-first (locate-library "replique")
    file-name-directory))




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
   :special-form "s"))

(defun replique/make-candidate (c)
  (map-let ((:candidate candidate) (:type type) (:package package) (:ns ns)) c
    (propertize candidate 'meta (replique/hash-map :type type :package package :ns ns))))

(defun replique/auto-complete* (prefix company-callback tooling-repl msg-type ns)
  (let ((tooling-chan (replique/get tooling-repl :chan)))
    (replique/send-tooling-msg
     tooling-repl
     (replique/hash-map :type msg-type
                  :context (replique/form-with-prefix)
                  :ns ns
                  :prefix prefix))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (if err
               (progn
                 (message "%s" (replique-edn/pr-str err))
                 (message "completion failed with prefix %s" prefix))
             (let* ((candidates (replique/get resp :candidates))
                    (candidates (mapcar 'replique/make-candidate candidates)))
               (funcall company-callback candidates)))))))))

(defun replique/auto-complete-session (prefix company-callback repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-type (replique/get repl :repl-type))
         (msg-type (cond ((equal :clj repl-type)
                          :clj-completion)
                         ((equal :cljs repl-type)
                          :cljs-completion)
                         (t (error "Invalid REPL type: %s" repl-type))))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/auto-complete* prefix company-callback tooling-repl msg-type ns)))

(defun replique/auto-complete-clj (prefix company-callback tooling-repl clj-repl)
  (when clj-repl
    (replique/auto-complete* prefix company-callback tooling-repl
                             :clj-completion (clojure-find-ns))))

(defun replique/auto-complete-cljs (prefix company-callback tooling-repl cljs-repl)
  (when cljs-repl
    (replique/auto-complete* prefix company-callback tooling-repl
                             :cljs-completion (clojure-find-ns))))

(defun replique/auto-complete-cljc (prefix company-callback tooling-repl clj-repl cljs-repl)
  (when (and clj-repl cljs-repl)
    (replique/auto-complete* prefix company-callback tooling-repl
                             :cljc-completion (clojure-find-ns))))

(defun replique/auto-complete (prefix company-callback)
  (when (not (null (replique/active-repl :tooling)))
    (replique/with-modes-dispatch
     (replique/mode . (-partial 'replique/auto-complete-session prefix company-callback))
     (clojure-mode . (-partial 'replique/auto-complete-clj prefix company-callback))
     (clojurescript-mode . (-partial 'replique/auto-complete-cljs prefix company-callback))
     (clojurec-mode . (-partial 'replique/auto-complete-cljc prefix company-callback)))))

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

(defun replique/repliquedoc-format-arglist (index arglist)
  (let ((args-index 1)
        (force-highlight nil))
    (thread-first
        (lambda (arg)
          (cond ((equal '& arg)
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

(defun replique/repliquedoc-format (msg)
  (map-let ((:name name) (:arglists arglists) (:return return) (:index index)) msg
    (cond ((and arglists return)
           (format "%s: %s -> %s"
                   name (replique/repliquedoc-format-arglists index arglists) return))
          (arglists
           (format "%s: %s" name (replique/repliquedoc-format-arglists index arglists)))
          (name (format "%s" name))
          (t nil))))

;; Eldoc expects eldoc-documentation-function to be synchronous. In order to turn the eldoc
;; response into an asynchronous one, we always return eldoc-last-message and call
;; eldoc-message ourselves later
(defun replique/repliquedoc* (tooling-repl msg-type ns)
  (let ((tooling-chan (replique/get tooling-repl :chan)))
    (when tooling-chan
      (replique/send-tooling-msg
       tooling-repl
       (replique/hash-map :type msg-type
                          :context (replique/form-with-prefix)
                          :ns ns
                          :symbol (symbol-name (symbol-at-point))))
      (replique-async/<!
       tooling-chan
       (lambda (resp)
         (when resp
           (let ((err (replique/get resp :error)))
             (if err
                 (progn
                   (message "%s" (replique-edn/pr-str err))
                   (message "eldoc failed"))
               (with-demoted-errors "eldoc error: %s"
                 (when-let ((msg (replique/repliquedoc-format (replique/get resp :doc))))
                   (eldoc-message msg))))))))))
  eldoc-last-message)

(defun replique/repliquedoc-session (repl)
  (let* ((directory (replique/get repl :directory))
         (tooling-repl (replique/repl-by :directory directory :repl-type :tooling))
         (repl-type (replique/get repl :repl-type))
         (msg-type (cond ((equal :clj repl-type)
                          :repliquedoc-clj)
                         ((equal :cljs repl-type)
                          :repliquedoc-cljs)
                         (t (error "Invalid REPL type: %s" repl-type))))
         (ns (symbol-name (replique/get repl :ns))))
    (replique/repliquedoc* tooling-repl msg-type ns)))

(defun replique/repliquedoc-clj (tooling-repl clj-repl)
  (when clj-repl
    (replique/repliquedoc* tooling-repl :repliquedoc-clj (clojure-find-ns))))

(defun replique/repliquedoc-cljs (tooling-repl cljs-repl)
  (when cljs-repl
    (replique/repliquedoc* tooling-repl :repliquedoc-cljs (clojure-find-ns))))

(defun replique/repliquedoc-cljc (tooling-repl clj-repl cljs-repl)
  (when (and clj-repl cljs-repl)
    (replique/repliquedoc* tooling-repl :repliquedoc-cljc (clojure-find-ns))))

(defun replique/eldoc-documentation-function ()
  ;; Ensures a REPL is started, since eldoc-mode is enabled globally by default
  (when (not (null (replique/active-repl :tooling)))
    (replique/with-modes-dispatch
     (replique/mode . 'replique/repliquedoc-session)
     (clojure-mode . 'replique/repliquedoc-clj)
     (clojurescript-mode . 'replique/repliquedoc-cljs)
     (clojurec-mode . 'replique/repliquedoc-cljc))))

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

(defun replique/in-ns-cljc (ns-name tooling-repl clj-repl cljs-repl)
  (if (not (and clj-repl cljs-repl))
      (user-error "No active Clojure AND Clojurescript REPL")
    (replique/send-input-from-source-cljc
     (format "(clojure.core/in-ns '%s)" ns-name)
     (format "(replique.interactive/cljs-in-ns '%s)" ns-name)
     tooling-repl clj-repl cljs-repl)))

(defun replique/in-ns (ns-name)
  (interactive (replique/symprompt "Set ns to" (clojure-find-ns)))
  (replique/with-modes-dispatch
   (clojure-mode . (-partial 'replique/in-ns-clj ns-name))
   (clojurescript-mode . (-partial 'replique/in-ns-cljs ns-name))
   (clojurec-mode . (-partial 'replique/in-ns-cljc ns-name))))

(defun replique/symbol-backward ()
  (let ((sym-bounds (bounds-of-thing-at-point 'symbol)))
    (when sym-bounds
      (buffer-substring-no-properties (car sym-bounds) (point)))))

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
   ((equal command 'candidates)
    `(:async . ,(-partial 'replique/auto-complete arg)))
   ((equal command 'annotation) (replique/auto-complete-annotation arg))))

(defun replique/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique/comint-send-input ()
  (interactive)
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
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

(defun replique/format-eval-message (msg)
  (if (replique/get msg :error)
      (let ((value (replique/get msg :value))
            (repl-type (replique/get msg :repl-type))
            (ns (replique/get msg :ns)))
        (if (string-prefix-p "Error:" value)
            (format "(%s) %s=> %s"
                    (replique/keyword-to-string repl-type) ns value)
          (format "(%s) %s=> Error: %s"
                  (replique/keyword-to-string repl-type) ns value)))
    (let ((result (replique/get msg :result))
          (repl-type (replique/get msg :repl-type))
          (ns (replique/get msg :ns)))
      (format "(%s) %s=> %s"
              (replique/keyword-to-string repl-type) ns result))))

(defun replique/display-eval-result (msg buff)
  ;; Concat to the current message in order to handle displaying results from cljc files
  ;; Note that (current-message) is cleared on any user action
  (when (not (seq-contains (replique/visible-buffers) buff))
    (let ((current-message (if (current-message)
                               (concat (current-message) "\n")
                             (current-message))))
      (->> (replique/format-eval-message msg)
           (concat current-message)
           replique/message-nolog))))

(defmacro replique/with-modes-dispatch (&rest modes-alist)
  (let* ((props-sym (make-symbol "props-sym"))
         (clj-repl-sym (make-symbol "clj-repl-sym"))
         (cljs-repl-sym (make-symbol "cljs-repl-sym"))
         (clj-buff-sym (make-symbol "clj-buff-sym"))
         (cljs-buff-sym (make-symbol "cljs-buff-sym"))
         (dispatch-code
          (mapcar
           (lambda (item)
             (let ((m (car item))
                   (f (cdr item)))
               ;; The order of priority is the order of the modes as defined during
               ;; the use of the macro
               (cond ((equal 'clojure-mode m)
                      `((equal 'clojure-mode major-mode)
                        (funcall ,f ,props-sym ,clj-repl-sym)))
                     ((equal 'clojurescript-mode m)
                      `((equal 'clojurescript-mode major-mode)
                        (if ,cljs-buff-sym
                            (funcall ,f ,props-sym ,cljs-repl-sym)
                          (user-error "No active Clojurescript REPL"))))
                     ((equal 'clojurec-mode m)
                      `((equal 'clojurec-mode major-mode)
                        (if (or ,clj-buff-sym ,cljs-buff-sym)
                            (funcall ,f ,props-sym
                                     ,clj-repl-sym ,cljs-repl-sym)
                          (user-error "No active Clojure or Clojurescript REPL"))))
                     ((equal 'css-mode m)
                      `((equal 'css-mode major-mode)
                        (if ,cljs-buff-sym
                            (funcall ,f ,props-sym ,cljs-repl-sym)
                          (user-error "No active Clojurescript REPL"))))
                     ((equal 'replique/mode m)
                      `((equal 'replique/mode major-mode)
                        (funcall ,f (replique/repl-by :buffer (current-buffer))))))))
           modes-alist))
         (dispatch-code (append dispatch-code
                                '((t (user-error
                                      "Cannot eval from major mode: %s"
                                      major-mode))))))
    `(let* ((,props-sym (replique/active-repl :tooling t))
            (,clj-repl-sym (replique/active-repl :clj))
            (,cljs-repl-sym (replique/active-repl :cljs))
            (,clj-buff-sym (replique/get ,clj-repl-sym :buffer))
            (,cljs-buff-sym (replique/get ,cljs-repl-sym :buffer)))
       (cond ,@dispatch-code))))

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
    (input-clj input-cljs tooling-repl clj-repl cljs-repl)
  (if (not (and clj-repl cljs-repl))
      (user-error "No active Clojure AND Clojurescript REPL")
    (let ((clj-buff (replique/get clj-repl :buffer))
          (cljs-buff (replique/get  cljs-repl :buffer)))
      (when clj-buff
        (with-current-buffer clj-buff
          (replique/comint-send-input-from-source input-clj)))
      (when cljs-buff
        (with-current-buffer cljs-buff
          (replique/comint-send-input-from-source input-cljs))))))

(defun replique/send-input-from-source-dispatch (input)
  (replique/with-modes-dispatch
   (clojure-mode . (-partial 'replique/send-input-from-source-clj input))
   (clojurescript-mode . (-partial'replique/send-input-from-source-cljs input))
   (clojurec-mode . (-partial'replique/send-input-from-source-cljc input input))))

(defun replique/eval-region (start end)
  "Eval the currently highlighted region."
  (interactive "r")
  (let ((input (filter-buffer-substring start end)))
    (replique/send-input-from-source-dispatch input)))

(defun replique/eval-last-sexp ()
  "Eval the previous sexp."
  (interactive)
  (replique/eval-region
   (save-excursion
     (clojure-backward-logical-sexp 1) (point))
   (point)))

(defun replique/eval-defn ()
  "Eval the current defn expression."
  (interactive)
  (replique/send-input-from-source-dispatch (thing-at-point 'defun)))

(defun replique/load-file-clj (file-path props clj-repl)
  (if (not clj-repl)
      (user-error "No active Clojure REPL")
    (replique/send-input-from-source-clj
     (format "(replique.interactive/load-file \"%s\")" file-path)
     props clj-repl)))

(defun replique/load-file-cljs (file-path props cljs-repl)
  (if (not cljs-repl)
      (user-error "No active Clojurescript REPL")
    (replique/send-input-from-source-cljs
     (format "(replique.interactive/load-file \"%s\")" file-path)
     props cljs-repl)))

(defun replique/load-file-cljc (file-path props clj-repl cljs-repl)
  (if (not (and clj-repl cljs-repl))
      (user-error "No active Clojure AND Clojurescript REPL")
    (replique/send-input-from-source-cljc
     (format "(replique.interactive/load-file \"%s\")" file-path)
     (format "(replique.interactive/load-file \"%s\")" file-path)
     props clj-repl cljs-repl)))

(defun replique/load-file ()
  "Load a file in a replique REPL"
  (interactive)
  (let ((file-path (buffer-file-name)))
    (comint-check-source file-path)
    (replique/with-modes-dispatch
     (clojure-mode . (-partial 'replique/load-file-clj file-path))
     (clojurescript-mode . (-partial 'replique/load-file-cljs file-path))
     (clojurec-mode . (-partial 'replique/load-file-cljc file-path)))))

(defun replique/browser ()
  (interactive)
  (let ((cljs-repl (replique/active-repl :cljs)))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (browse-url (format "http://localhost:%s" (replique/get cljs-repl :port)))))

(defun replique/switch-active-repl (repl-buff-name)
  "Switch the currently active REPL"
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
     (list (ido-completing-read "Switch to REPL: " repl-names nil t))))
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
  "Switch the currently active process"
  (interactive
   (let* ((tooling-repls (replique/repls-by :repl-type :tooling))
          (directories (mapcar (lambda (repl) (replique/get repl :directory))
                               tooling-repls)))
     (when (not (car directories))
       (user-error "No started REPL"))
     (list (ido-completing-read "Switch to process: " directories nil t))))
  (let* ((new-active-repls (replique/repls-by :directory proc-name))
         (prev-active-tooling-repl (replique/active-repl :tooling))
         (prev-active-directory (replique/get prev-active-tooling-repl :directory))
         (prev-repls (replique/repls-by :directory prev-active-directory))
         (prev-repl-types (mapcar (lambda (repl) (replique/get repl :repl-type)) prev-repls)))
    ;; All windows displaying the previously active repls are set to display the newly active
    ;; repls (repl with the same repl-type and same position in the replique/repls list)
    (mapcar (lambda (repl-type)
              (let* ((prev-active-repl (replique/repl-by :directory prev-active-directory
                                                         :repl-type repl-type))
                     (prev-buffer (replique/get prev-active-repl :buffer))
                     (new-active-repl (replique/repl-by
                                       :directory proc-name
                                       :repl-type repl-type))
                     (new-buffer (replique/get new-active-repl :buffer))
                     (windows (get-buffer-window-list prev-buffer)))
                (when new-buffer
                  (mapcar (lambda (window)
                            (set-window-buffer window new-buffer))
                          windows))))
            prev-repl-types)
    (mapcar (lambda (repl)
              (setq replique/repls (delete repl replique/repls))
              (setq replique/repls (push repl replique/repls)))
            (seq-reverse new-active-repls))))

(defun replique/list-cljs-namespaces (tooling-repl)
  (let ((tooling-chan (replique/get tooling-repl :chan))
        (out-chan (replique-async/chan)))
    (replique/send-tooling-msg
     tooling-repl
     (replique/hash-map :type :list-cljs-namespaces))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (if err
               (progn
                 (message "%s" (replique-edn/pr-str err))
                 (message "list-cljs-namespaces failed")
                 ;; TODO test me !
                 (replique-async/close! out-chan))
             (replique-async/put! out-chan resp))))))
    out-chan))

(defun replique/output-main-cljs-file (output-to &optional main-ns)
  (interactive (list nil nil))
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (tooling-chan (replique/get tooling-repl :chan))
         (cljs-repl (replique/active-repl :cljs)))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (when-let ((output-to (or
                           output-to
                           (read-file-name "Output main cljs file to: "
                                           (replique/get cljs-repl :directory)))))
      (let ((output-to (file-truename output-to)))
        (when (or (not (file-exists-p output-to))
                  (yes-or-no-p (format "Override %s?" output-to))) 
          (replique-async/<!
           (if main-ns
               (replique-async/default-chan (replique/hash-map :namespaces nil))
             (replique/list-cljs-namespaces tooling-repl))
           (lambda (resp)
             (when resp
               (let* ((namespaces (replique/get resp :namespaces))
                      (main-ns (or main-ns
                                   (replique/return-nil-on-quit
                                    (completing-read "Main Clojurescript namespace: "
                                                     namespaces nil t nil nil '(nil))))))
                 (replique/send-tooling-msg
                  tooling-repl
                  (replique/hash-map :type :output-main-cljs-files
                                     :main-cljs-files (replique/hash-map output-to main-ns)))
                 (replique-async/<!
                  tooling-chan
                  (lambda (resp)
                    (when resp
                      (let ((err (replique/get resp :error)))
                        (if err
                            (progn
                              (message "%s" (replique-edn/pr-str err))
                              (message "output-main-cljs-file failed"))
                          (message "Main Clojurescript file written to: %s" output-to)
                          (thread-last 
                              (replique/update-in tooling-repl [:main-cljs-files]
                                                  'replique/assoc output-to main-ns)
                            (replique/update-repl tooling-repl))
                          (replique/save-repls
                           (replique/get tooling-repl :directory))))))))))))))))

(defun replique/list-css (tooling-repl)
  (let ((tooling-chan (replique/get tooling-repl :chan))
        (out-chan (replique-async/chan)))
    (replique/send-tooling-msg
     tooling-repl
     (replique/hash-map :type :list-css))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (if err
               (progn
                 (message "%s" (replique-edn/pr-str err))
                 (message "list-css failed")
                 (replique-async/close! out-chan))
             (replique-async/put! out-chan resp))))))
    out-chan))

(defun replique/load-css (file-name)
  (let* ((tooling-repl (replique/active-repl :tooling t))
         (tooling-chan (replique/get tooling-repl :chan))
         (cljs-repl (replique/active-repl :cljs)))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (replique-async/<!
     (replique/list-css tooling-repl)
     (lambda (resp)
       (when resp
         (print resp))))))

(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

(defcustom replique/lein-script "/Users/egr/bin/lein"
  "Leiningen script path"
  :type 'file
  :group 'replique)

(defcustom replique/clojure-jar "/Users/egr/bin/clojure-1.9.0-alpha11.jar"
  ""
  :type 'file
  :group 'replique)

(defcustom replique/clojurescript-jar nil
  ""
  :type 'file
  :group 'replique)

;; TODO check the hashmap type
(defcustom replique/files-specs
  (replique/hash-map)
  ""
  :type '(restricted-sexp :match-alternatives (hash-table-p 'nil))
  :group 'replique)

(defcustom replique/cljs-compile-path "target-cljs"
  ""
  :type 'string
  :group 'replique)

(defvar replique/mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" 'replique/comint-send-input)
    (define-key map "\C-xr" 'replique/switch-active-repl)
    map))

(define-derived-mode replique/mode comint-mode "Replique"
  "Commands:\\<replique/mode-map>"
  (setq-local comint-prompt-regexp replique/prompt)
  (setq-local comint-prompt-read-only t)
  (setq-local mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (set-syntax-table clojure-mode-syntax-table)
  (add-to-list 'company-backends 'replique/company-backend)
  (add-function :before-until (local 'eldoc-documentation-function)
                'replique/eldoc-documentation-function)
  (setq-local company-tooltip-align-annotations t))

(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'replique/eval-region)
    (define-key map "\C-x\C-e" 'replique/eval-last-sexp)
    (define-key map "\C-\M-x" 'replique/eval-defn)
    (define-key map "\C-c\C-l" 'replique/load-file)
    (define-key map "\C-c\M-n" 'replique/in-ns)
    (define-key map "\C-xr" 'replique/switch-active-repl)
    (define-key map "\M-." 'replique/jump-to-definition)
    (define-key map "\M-," 'pop-tag-mark)
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
        ))
    map))

(defvar replique/generic-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map))

;;;###autoload
(define-minor-mode replique/minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/minor-mode-map}"
  :lighter "Replique" :keymap replique/minor-mode-map
  (add-to-list 'company-backends 'replique/company-backend)
  (add-function :before-until (local 'eldoc-documentation-function)
                'replique/eldoc-documentation-function)
  (setq-local company-tooltip-align-annotations t))

(defun replique/raw-command-classpath ()
  (cond ((and (null replique/clojure-jar)
              (null replique/clojurescript-jar))
         (error "None of the clojure or clojurescript jar have been configured"))
        ((null replique/clojurescript-jar)
         (format "%s:%sclj/src" replique/clojure-jar (replique/replique-root-dir)))
        ((null replique/clojure-jar)
         (format "%s:%sclj/src" replique/clojurescript-jar (replique/replique-root-dir)))
        (t
         (format "%s:%s:%sclj/src"
                 replique/clojure-jar replique/clojurescript-jar
                 (replique/replique-root-dir)))))

(defun replique/raw-command (directory port)
  `("java" "-cp" ,(replique/raw-command-classpath) "clojure.main" "-m" "replique.main"
    ,(format "{:type :clj :port %s :directory %s :replique-vars {:files-specs %s :cljs-compile-path %s}}"
             (number-to-string port) (replique-edn/pr-str directory)
             (replique-edn/pr-str replique/files-specs)
             (replique-edn/pr-str replique/cljs-compile-path))))

(defun replique/lein-command (directory port)
  `(,(or replique/lein-script "lein") "update-in" ":source-paths" "conj"
    ,(format "\"%sclj/src\"" (replique/replique-root-dir))
    "--" "run" "-m" "replique.main/-main"
    ,(format "{:type :clj :port %s :directory %s :replique-vars {:files-specs %s :cljs-compile-path %s}}"
             (number-to-string port) (replique-edn/pr-str directory)
             (replique-edn/pr-str replique/files-specs)
             (replique-edn/pr-str replique/cljs-compile-path))))

(defun replique/is-lein-project (directory)
  (file-exists-p (expand-file-name "project.clj" directory)))

(defun replique/process-filter-chan (proc)
  (let ((chan (replique-async/chan)))
    (set-process-filter
     proc (lambda (proc string)
            (replique-async/put! chan string)))
    chan))

(defun replique/is-valid-port-nb? (port-nb)
  (< -1 port-nb 65535))

(defun replique/send-tooling-msg (tooling-repl msg)
  (let* ((tooling-network-proc (replique/get tooling-repl :network-proc))
         (directory (replique/get tooling-repl :directory))
         (msg (replique/assoc msg :directory directory)))
    (process-send-string
     tooling-network-proc
     (format "(replique.tooling-msg/tooling-msg-handle %s)\n"
             (replique-edn/pr-str msg)))))

;; Note: a tooling repl may not be closed when there is a pending non daemon thread
;; We don't want to save REPLs when closing the process, as opposed to closing a single REPL
(defun replique/close-tooling-repl (repl &optional dont-save-repls?)
  (let ((tooling-proc (replique/get repl :proc))
        (tooling-chan (replique/get repl :chan)))
    (when (process-live-p tooling-proc)
      (interrupt-process tooling-proc))
    (setq replique/repls (delete repl replique/repls))))

(defun replique/close-repl (repl-props &optional dont-save-repls?)
  (let* ((buffer (replique/get repl-props :buffer))
         (host (replique/get repl-props :host))
         (port (replique/get repl-props :port))
         (directory (replique/get repl-props :directory))
         (proc (get-buffer-process buffer)))
    (when proc
      (set-process-sentinel proc nil)
      (delete-process proc))
    (kill-buffer buffer)
    (setq replique/repls (delete repl-props replique/repls))
    (when (not dont-save-repls?)
      (replique/save-repls directory))
    ;; If the only repl left is a tooling repl, thus let's close it
    (let ((other-repls (replique/repls-by :host host :port port)))
      (when (seq-every-p (lambda (repl)
                           (equal :tooling (replique/get repl :repl-type)))
                         other-repls)
        (mapcar (lambda (tooling-repl)
                  (let ((tooling-chan (replique/get tooling-repl :chan)))
                    (when (process-live-p (replique/get tooling-repl :network-proc))
                        (replique/send-tooling-msg
                         tooling-repl (replique/hash-map :type :shutdown))
                      (replique-async/<!
                       tooling-chan
                       (lambda (msg)
                         (when (and msg (replique/get msg :error))
                           (error "Error while shutting down the REPL: %s"
                                  (replique-edn/pr-str (replique/get msg :error)))))))))
                other-repls)))))

(defun replique/close-repls (host port &optional dont-save-repls?)
  (let* ((repls (replique/repls-by :host host :port port))
         (not-tooling-repls (seq-filter (lambda (repl)
                                       (not (equal :tooling (replique/get repl :repl-type))))
                                     repls))
         (tooling-repls (seq-filter (lambda (repl)
                                   (equal :tooling (replique/get repl :repl-type)))
                                 repls)))
    (mapcar (lambda (repl)
              (replique/close-repl repl dont-save-repls?))
            not-tooling-repls)
    (mapcar (lambda (repl)
              (replique/close-tooling-repl repl))
            tooling-repls)))

(defun replique/on-tooling-repl-close (host port process event)
  (replique/kill-process-buffer process)
  (cond ((string= "deleted\n" event)
         (replique/close-repls host port))
        ((string= "connection broken by remote peer\n" event)
         (replique/close-repls host port))
        (t nil)))

;; java -cp "/Users/egr/bin/clojure-1.8.0.jar:/Users/egr/bin/cljs-1.9.216.jar:/Users/egr/replique.el/clj/src" clojure.main -m replique.main "{:type :clj :port 0}"

(defun replique/dispatch-repl-cmd (directory port)
  (if (replique/is-lein-project directory)
      (replique/lein-command directory port)
    (replique/raw-command directory port)))

(defun replique/is-in-exec-path (file absolute?)
  (thread-first (seq-mapcat
                 (lambda (dir)
                   (directory-files dir absolute?))
                 exec-path)
    (seq-contains file)))

(defun replique/repl-cmd-pre-cond (directory)
  (let ((is-lein-project (replique/is-lein-project directory)))
    (cond ((and is-lein-project
                replique/lein-script
                (not (replique/is-in-exec-path replique/lein-script t)))
           (message "Error while starting the REPL. %s could not be find in exec-path. Please update replique/lein-script" replique/lein-script)
           nil)
          ((and is-lein-project
                (not replique/lein-script)
                (not (replique/is-in-exec-path "lein" nil)))
           (message "Error while starting the REPL. No lein script found in exec-path")
           nil)
          ((and (not is-lein-project)
                (not replique/clojure-jar)
                (not replique/clojurescript-jar))
           (message "Error while starting the REPL. None of replique/clojure-jar or replique/clojurescript-jar have been customized")
           nil)
          ((and (not is-lein-project)
                replique/clojure-jar
                (not (replique/is-in-exec-path replique/clojure-jar t)))
           (message "Error while starting the REPL. %s could not be find in exec-path. Please update replique/clojure-jar" replique/clojure-jar)
           nil)
          ((and (not is-lein-project)
                replique/clojurescript-jar
                (not (replique/is-in-exec-path replique/clojurescript-jar t)))
           (message "Error while starting the REPL. %s could not be find in exec-path. Please update replique/clojurescript-jar" replique/clojurescript-jar)
           nil)
          (t t))))

(defun replique/skip-repl-starting-output* (proc-chan filtered-chan &optional filtered?)
  (replique-async/<!
   proc-chan (lambda (msg)
               ;; REPL has already been started
               (cond (filtered?
                      (replique-async/put! filtered-chan msg)
                      (replique/skip-repl-starting-output* proc-chan filtered-chan t))
                     ;; Check if the REPL is starting, or, otherwise, if the process is still
                     ;; printing init messages
                     ((string-match-p (regexp-quote "Starting Clojure REPL...\n") msg)
                      (let ((splitted-msg (split-string msg "Starting Clojure REPL...\n")))
                        (message "%s" (car splitted-msg))
                        (replique-async/put! filtered-chan (cadr splitted-msg))
                        (replique/skip-repl-starting-output* proc-chan filtered-chan t)))
                     ;; Print all the init messages
                     (t
                      (message "%s" msg)
                      (replique/skip-repl-starting-output* proc-chan filtered-chan))))))

(defun replique/skip-repl-starting-output (proc-chan)
  (let ((filtered-chan (replique-async/chan)))
    (replique/skip-repl-starting-output* proc-chan filtered-chan)
    filtered-chan))

(defun replique/is-main-cljs-file (f)
  (let* ((first-line "//main-cljs-file autogenerated by replique")
         (check-length (length first-line)))
    (and (file-exists-p f)
         (condition-case err
             (with-temp-buffer
               (insert-file-contents f nil 0 check-length)
               (equal (buffer-substring-no-properties 1 (+ 1  check-length)) first-line))
           (error nil)))))

(defun replique/refresh-main-cljs-files (tooling-repl main-cljs-files)
  (let ((tooling-chan (replique/get tooling-repl :chan))
        (out-chan (replique-async/chan))
        (filtered-mains (replique/hash-map)))
    (maphash (lambda (k v)
               (when (replique/is-main-cljs-file k)
                 (puthash k v filtered-mains)))
             (or main-cljs-files (replique/hash-map)))
    (replique/send-tooling-msg
     tooling-repl
     (replique/hash-map :type :output-main-cljs-files
                        :main-cljs-files filtered-mains))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (replique/get resp :error)))
           (when err
             (message "%s" (replique-edn/pr-str err))
             (message "Error while refreshing main cljs files")))
         (replique-async/put!
          out-chan (replique/assoc tooling-repl :main-cljs-files filtered-mains)))))
    out-chan))

(defun replique/make-tooling-repl (host port directory main-cljs-files out-chan)
  (let* ((default-directory directory)
         (random-port? (equal port 0))
         (repl-cmd (replique/dispatch-repl-cmd directory port))
         (proc (apply 'start-process directory (format " *%s*" directory)
                      (car repl-cmd) (cdr repl-cmd)))
         (proc-chan (replique/skip-repl-starting-output (replique/process-filter-chan proc)))
         (chan (replique/read-chan
                proc-chan proc
                (lambda (proc-out-s)
                  (error "Error while starting the REPL: %s" proc-out-s)))))
    (set-process-sentinel proc (lambda (proc string) (replique/kill-process-buffer proc)))
    (replique-async/<!
     chan (lambda (repl-infos)
            (replique/kill-process-buffer proc)
            ;; Print messages from unbound threads
            (set-process-filter proc (lambda (proc string)
                                       (message "Process %s: %s" (process-name proc) string)))
            (if (replique/get repl-infos :error)
                (message "Error while starting the REPL: %s"
                         (replique-edn/pr-str (replique/get repl-infos :error)))
              (let* ((host (replique/get repl-infos :host))
                     (port (replique/get repl-infos :port))
                     (network-proc (open-network-stream directory (format " *%s*" directory)
                                                        host port))
                     (tooling-chan (replique/process-filter-chan network-proc)))
                (set-process-sentinel
                 network-proc (-partial 'replique/on-tooling-repl-close host port))
                ;; The REPL accept fn will read a char in order to check whether the request
                ;; is an HTTP one or not
                (process-send-string network-proc "R")
                ;; No need to wait for the return value of shared-tooling-repl
                ;; since it does not print anything
                (process-send-string
                 network-proc "(replique.repl/shared-tooling-repl)\n")
                (let* ((tooling-chan (-> tooling-chan
                                         (replique/read-chan network-proc)
                                         replique/dispatch-tooling-msg))
                       (tooling-repl (replique/hash-map
                                      :directory directory
                                      :repl-type :tooling
                                      :proc proc
                                      :network-proc network-proc
                                      :host host
                                      :port port
                                      :random-port? random-port?
                                      :chan tooling-chan)))
                  ;; Refresh main-cljs-files with the new port number
                  (replique-async/<!
                   (replique/refresh-main-cljs-files tooling-repl main-cljs-files)
                   (lambda (tooling-repl)
                     (push tooling-repl replique/repls)
                     (replique-async/put! out-chan tooling-repl))))))))))

(defun replique/on-repl-close (host port buffer process event)
  (let ((closing-repl (replique/repl-by :host host :port port :buffer buffer)))
    (when closing-repl
      (cond ((string= "deleted\n" event)
             (replique/close-repl closing-repl))
            ((string= "connection broken by remote peer\n" event)
             (with-current-buffer buffer
               (save-excursion
                 (goto-char (point-max))
                 (insert (concat "\n" event "\n"))))
             (replique/close-repl closing-repl))
            (t nil)))))

(defun replique/close-process (directory)
  (interactive
   (let* ((tooling-repls (replique/repls-by :repl-type :tooling :error-on-nil t))
          (directories (mapcar (lambda (repl) (replique/get repl :directory)) tooling-repls)))
     (list (ido-completing-read "Close process: " directories nil t))))
  (let* ((tooling-repl (replique/repl-by :repl-type :tooling :directory directory))
         (host (replique/get tooling-repl :host))
         (port (replique/get tooling-repl :port)))
    ;; When closing the process, we don't override REPLs saved on disk, by contrast with
    ;; closing a single REPL
    (replique/close-repls host port t)))

(defun replique/make-comint (proc buffer)
  (with-current-buffer buffer
    (unless (derived-mode-p 'comint-mode)
      (comint-mode))
    (set-process-filter proc 'comint-output-filter)
    (setq-local comint-ptyp process-connection-type) ; t if pty, nil if pipe.
    ;; Jump to the end, and set the process mark.
    (goto-char (point-max))
    (set-marker (process-mark proc) (point))
    (run-hooks 'comint-exec-hook)
    buffer))

(defun replique/make-repl (buffer-name directory host port repl-type bindings)
  (let* ((buff (get-buffer-create buffer-name))
         (proc (open-network-stream buffer-name buff host port))
         (chan-src (replique/process-filter-chan proc))
         (repl-cmd (format "(replique.repl/repl)\n")))
    (set-process-sentinel proc (-partial 'replique/on-repl-close host port buff))
    ;; The REPL accept fn will read a char in order to check whether the request
    ;; is an HTTP one or not
    (process-send-string proc "R")
    (let ((chan (replique/read-chan chan-src proc)))
      ;; Restore bindings and get the session number
      ;; The forms are wrapped in a do to avoid the need to read the result
      ;; of restore-bindings
      (process-send-string proc (format "(replique.repl/init-and-print-session %s)\n"
                                        (replique-edn/pr-str bindings)))
         (replique-async/<!
          chan
          (lambda (resp)
            (let ((session (replique/get resp :client)))
              (set-process-filter proc nil)
              (replique/make-comint proc buff)
              (set-buffer buff)
              (replique/mode)
              (process-send-string proc repl-cmd)
              (let ((repl (replique/hash-map :directory directory
                                             :host host
                                             :port port
                                             :repl-type repl-type
                                             :session session
                                             :ns 'user
                                             :buffer buff
                                             :bindings bindings)))
                (push repl replique/repls)
                (replique/save-repls directory)
                ;; Save REPL props when its name changes. The hook will run for interactive
                ;; commands only
                (with-current-buffer buff
                  (add-hook 'post-command-hook
                            (lambda ()
                              (when (equal 'rename-buffer this-command)
                                (replique/save-repls directory)))
                            t t))
                (when (equal repl-type :cljs)
                  ;; Second parameter is nil because the function does not use the
                  ;; tooling repl anyway
                  (replique/send-input-from-source-clj
                   "(replique.interactive/cljs-repl)" nil repl))
                (display-buffer buff))))))))

(defun replique/clj-buff-name (directory repl-type)
  (let ((repl-type-string (replique/keyword-to-string repl-type)))
    (generate-new-buffer-name
     (format "*replique*%s*%s*"
             (file-name-nondirectory (directory-file-name directory))
             repl-type-string))))

(defun replique/save-repls (directory)
  ;; Processes started from a symbolic link are not persisted
  (when (equal (file-truename directory) directory)
    (let* ((repls (replique/repls-by :directory directory))
           (repls-directory (concat directory ".replique/"))
           (repls-file-name (concat repls-directory "repls")))
      (if (or
           (null repls)
           (and (equal 1 (length repls))
                (equal :tooling (replique/get (car repls) :repl-type))))
          (when (file-exists-p repls-file-name)
            (delete-file repls-file-name))
        (make-directory repls-directory t)
        (with-temp-file repls-file-name
          (let* ((repls (mapcar (lambda (repl)
                                  (let ((host (replique/get repl :host))
                                        (port (replique/get repl :port))
                                        (repl-type (replique/get repl :repl-type))
                                        (random-port? (replique/get repl :random-port?))
                                        (buffer (replique/get repl :buffer))
                                        (bindings (replique/get repl :bindings))
                                        (main-cljs-files (replique/get repl :main-cljs-files)))
                                    (if (equal repl-type :tooling)
                                        (replique/hash-map :host host :port port
                                                           :random-port? random-port?
                                                           :repl-type repl-type
                                                           :main-cljs-files main-cljs-files)
                                      (replique/hash-map :repl-type repl-type
                                                         :buffer-name (buffer-name buffer)
                                                         :bindings bindings))))
                                repls))
                 (repls-string (with-output-to-string (pp repls))))
            (insert repls-string)))))))

(defun replique/validate-saved-repls (validated saved-repls)
  (if (null saved-repls)
      validated
    (when (hash-table-p (car saved-repls))
      (let* ((saved-repl (car saved-repls))
             (repl-type (replique/get saved-repl :repl-type))
             (host (replique/get saved-repl :host))
             (port (replique/get saved-repl :port))
             (random-port? (replique/get saved-repl :random-port?))
             (buffer-name (replique/get saved-repl :buffer-name))
             (bindings (replique/get saved-repl :bindings)))
        (cond ((and (equal repl-type :tooling)
                    (not (null host)) (stringp host)
                    (replique/is-valid-port-nb? port))
               (replique/validate-saved-repls
                (cons saved-repl validated) (cdr saved-repls)))
              ((and (or (equal :clj repl-type) (equal :cljs repl-type))
                    (not (null buffer-name)) (stringp buffer-name)
                    (or (null bindings) (hash-table-p bindings)))
               (replique/validate-saved-repls
                (cons saved-repl validated) (cdr saved-repls)))
              ;; This is an error, return nil
              (t nil))))))

;; Returns nil in case of an error
;; Return 0 as port number in case of a random port
(defun replique/read-saved-repls (directory)
  ;; Processes started from a symbolic link are not persisted
  (when (equal (file-truename directory) directory)
    (let ((repls-file-name (concat directory ".replique/repls")))
      (when (file-exists-p repls-file-name)
        (let* ((saved-repls (with-temp-buffer
                              (insert-file-contents repls-file-name)
                              (buffer-string)))
               (saved-repls (condition-case err
                                (read saved-repls)
                              (error (message "Error while reading saved REPLs: %s"
                                              (error-message-string err))
                                     nil))))
          (replique/validate-saved-repls nil saved-repls))))))

(defun replique/normalize-directory-name (directory)
  (file-name-as-directory (expand-file-name directory)))

;; Port number is ignored when a REPL process is already existing - the existing REPL port
;; number is used instead
;;;###autoload
(defun replique/repl (&optional directory host port)
  (interactive
   (let ((directory (read-directory-name
                     "Project directory: " (replique/guess-project-root-dir) nil t)))
     ;; Normalizing the directory name is necessary in order to be able to search repls
     ;; by directory name
     (list (replique/normalize-directory-name directory))))
  (if (not (replique/repl-cmd-pre-cond directory))
      nil
    (let* ((existing-repl (replique/repl-by
                           :directory directory
                           :repl-type :tooling))
           (saved-repls (if existing-repl nil (replique/read-saved-repls directory)))
           (tooling-repl-chan (replique-async/chan)))
      (if existing-repl
          (replique-async/put! tooling-repl-chan existing-repl)
        (let* ((saved-tooling-repl (replique/get-by saved-repls :repl-type :tooling))
               (saved-host (replique/get saved-tooling-repl :host))
               (saved-port (replique/get saved-tooling-repl :port))
               (random-port? (replique/get saved-tooling-repl :random-port?))
               (host (or host saved-host "localhost"))
               (port (or port (if random-port? 0 saved-port) (read-number "Port number: " 0)))
               (main-cljs-files (replique/get saved-tooling-repl :main-cljs-files)))
          (if (not (replique/is-valid-port-nb? port))
              (user-error "Invalid port number: %d" port)
            (replique/make-tooling-repl host port directory main-cljs-files tooling-repl-chan))))
      (replique-async/<!
       tooling-repl-chan
       (-> (lambda (saved-repls repl)
             (let ((directory (replique/get repl :directory))
                   (host (replique/get repl :host))
                   (port (replique/get repl :port))
                   (tooling-chan (replique/get repl :chan)))
               (if saved-repls
                   (mapcar (lambda (repl)
                             (let ((repl-type (replique/get repl :repl-type))
                                   (buffer-name (replique/get repl :buffer-name))
                                   (bindings (replique/get repl :bindings)))
                               (when (not (equal repl-type :tooling))
                                 ;; make-repl expects buffer-name to be the name of no already
                                 ;; existing buffer
                                 (replique/make-repl (generate-new-buffer-name buffer-name)
                                                     directory host port repl-type
                                                     bindings))))
                           saved-repls)
                 (let* ((buff-name (replique/clj-buff-name directory :clj)))
                   (replique/make-repl buff-name directory host port :clj nil)))))
           (-partial saved-repls))))))

(defun replique/on-repl-type-change (repl new-repl-type)
  (let* ((directory (replique/get repl :directory))
         (repl-type (replique/get repl :repl-type))
         (repl-type-s (replique/keyword-to-string repl-type))
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
            (rename-buffer (generate-new-buffer-name new-buffer-name)))))
      (replique/update-repl repl (replique/assoc repl :repl-type new-repl-type))
      (replique/save-repls directory))))

(defun replique/dispatch-tooling-msg* (in-chan out-chan)
  (replique-async/<!
   in-chan
   (lambda (msg)
     (condition-case err
         (cond
          ((not (hash-table-p msg))
           (message "Received invalid message while dispatching tooling messages: %s" msg))
          (;; Global error (uncaught exception)
           (and
            (replique/get msg :error)
            (replique/get msg :thread))
           (message "%s - Thread: %s - Exception: %s"
                    (propertize "Uncaught exception" 'face '(:foreground "red"))
                    (replique/get msg :thread)
                    (replique-edn/pr-str (replique/get msg :value))))
          ((equal :binding (replique/get msg :type))
           (let* ((repl (replique/repl-by
                         :session (replique/get (replique/get msg :session) :client)
                         :directory (replique/get msg :directory)))
                  (buffer (replique/get repl :buffer))
                  (var (replique/get msg :var)))
             (when repl
               (thread-last
                   (replique/get msg :value)
                 (replique/assoc-in repl `[:bindings ,var])
                 (replique/update-repl repl))
               (replique/save-repls (replique/get msg :directory)))))
          ((equal :eval (replique/get msg :type))
           (let* ((repl (replique/repl-by
                         :session (replique/get (replique/get msg :session) :client)
                         :directory (replique/get msg :directory)))
                  (buffer (replique/get repl :buffer)))
             (when repl
               (replique/on-repl-type-change repl (replique/get msg :repl-type))
               (replique/update-repl repl (replique/assoc repl :ns (replique/get msg :ns)))
               (replique/display-eval-result msg buffer))))
          (t (replique-async/put! out-chan msg)))
       (error (message (error-message-string err))))
     (replique/dispatch-tooling-msg* in-chan out-chan))))

(defun replique/dispatch-tooling-msg (in-chan)
  (let ((out-chan (replique-async/chan)))
    (replique/dispatch-tooling-msg* in-chan out-chan)
    out-chan))

(defun replique/read-from-to (chan-out from to)
  (goto-char from)
  (when-let ((o (read (current-buffer))))
    (replique-async/put! chan-out o))
  (while (< (point) to)
    (when-let ((o (read (current-buffer))))
      (replique-async/put! chan-out o))))

(defun replique/read-buffer (chan-out)
  (let ((new-parser-state (parse-partial-sexp
                           (point) (point-max) 0 nil replique/parser-state)))
    (if (not (= 0 (car new-parser-state)))
        (setq replique/parser-state new-parser-state)
      (setq replique/parser-state nil)
      (condition-case err
          (let ((to (point)))
            (replique/read-from-to chan-out 1 to)
            (delete-region 1 to)
            (replique/read-buffer chan-out))
        (end-of-file (delete-region 1 (point-max)))))))

(defun replique/read-chan* (chan-in chan-out proc &optional error-handler)
  (replique-async/<!
   chan-in
   (lambda (string)
     (when string
       (condition-case err
           (when (buffer-live-p (process-buffer proc))
             (with-current-buffer (process-buffer proc)
               (let ((p (point)))
                 (insert string)
                 (goto-char p))
               (replique/read-buffer chan-out))
             (replique/read-chan* chan-in chan-out proc error-handler))
         (error
          (if error-handler
              (funcall error-handler string)
            (error (error-message-string err)))))))))

(defun replique/read-chan (chan-in proc &optional error-handler)
  (let ((chan-out (replique-async/chan))
        (parser-state nil))
    (with-current-buffer (process-buffer proc)
      (setq-local replique/parser-state nil))
    (replique/read-chan* chan-in chan-out proc error-handler)
    chan-out))

(defun replique/kill-process-buffer (proc)
  (let ((proc-buffer (process-buffer proc)))
    (set-process-buffer proc nil)
    (when proc-buffer
      (kill-buffer proc-buffer))))

(provide 'replique2)

;; jump to definition
;; Epresent
;; css, js
;; sourcepath, classpath live reload
;; var explorer
;; exceptions explorer (fold/unfold?)
;; compliment keywords cljs -> missing :require ... ?
;; remove emacs auto save files
;; check for nil when reading from chan because the chan can be closed
;; CSS / HTML autocompletion, with core.spec ?
;; support for no cljs-env
;; We must print the full exception infos to the tooling channel in order to make an exception explorer
;; Customizing REPL options requires starting a new REPL (leiningen options don't work in the context of replique). Find a way to automate this process (using leiningen or not ...)
;; multi-process -> print directory in messages
;; Set the asset path in repl-env on :ready because it is used elsewhere (parsestacktrace ...), bot webapp-env and browser-env when using the main file
;; The cljs-env makes no use of :repl-require
;; autocomplete interactive command

;; compliment invalidate memoized on classpath update
;; Use a lein task to compute the new classpath and send it to the clojure process.

;; autocomplete using the spec first, compliment next if no candidates

;; Normalize file path for *files-specs*
;; new cljs tagged reader

;; replique.el ends here
