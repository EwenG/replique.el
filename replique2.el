;;; replique2.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.12.0") (company "0.9.0") (dash-functional "1.2.0") (s "1.9.0"))
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'replique-edn)
(require 'replique-async)
(require 'company)
(require 'dashhash)
(require 'repliquedoc)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/keyword-to-string (k)
  (substring (symbol-name k) 1))

(defun replique/message-nolog (format-string &rest args)
  (let ((message-log-max nil))
    (apply 'message format-string args)))

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
  (->> replique/repls
       (mapcar (lambda (repl)
                 (let* ((repl (if (-h/contains? repl :buffer)
                                  (-h/assoc repl :buffer '**)
                                repl))
                        (repl (if (-h/contains? repl :proc)
                                  (-h/assoc repl :proc '**)
                                repl))
                        (repl (if (-h/contains? repl :network-proc)
                                  (-h/assoc repl :network-proc '**)
                                repl))
                        (repl (if (-h/contains? repl :chan)
                                  (-h/assoc repl :chan '**)
                                repl))
                        (repl (if (-h/contains? repl :eval-chan)
                                  (-h/assoc repl :eval-chan '**)
                                repl)))
                   repl)))
       (mapcar 'replique-edn/pr-str)
       (s-join "\n")))

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

(comment
 (defun replique/repls-or-repl-by (filtering-fn source &rest args)
   (-let* ((pred (lambda (repl-props)
                   (and
                    (-all? (-lambda ((k . v))
                             (or (equal :error-on-nil k)
                                 (not (plist-member args k))
                                 (equal (plist-get args k) v)))
                           repl-props)
                    (-all? (-lambda ((k . v))
                             (-any? (-lambda ((repl-k . repl-v))
                                      (or (equal :error-on-nil k)
                                          (equal k repl-k)))
                                    repl-props))
                           (replique/plist->alist args)))))
           (found (funcall filtering-fn pred source)))
     (if (and (null found) (plist-get args :error-on-nil))
         (user-error "No started REPL")
       found)))
 )

(defun replique/repls-or-repl-by (filtering-fn source &rest args)
  (-let* ((pred (lambda (repl)
                  (-all? (-lambda ((k . v))
                           (or (equal :error-on-nil k)
                               (and (-h/contains? repl k)
                                    (equal v (-h/get repl k)))))
                         (replique/plist->alist args))))
          (found (funcall filtering-fn pred source)))
    (if (and (null found) (plist-get args :error-on-nil))
        (user-error "No started REPL")
      found)))

(defun replique/repl-by (&rest args)
  (apply 'replique/repls-or-repl-by '-first replique/repls args))

(defun replique/repls-by (&rest args)
  (apply 'replique/repls-or-repl-by '-filter replique/repls args))

(defun replique/get-by (source &rest args)
  (apply 'replique/repls-or-repl-by '-first source args))

(defun replique/get-all-by (source &rest args)
  (apply 'replique/repls-or-repl-by '-filter source args))

(defun replique/active-repl (repl-type &optional error-on-nil)
  (if error-on-nil
      (replique/repl-by :repl-type repl-type :error-on-nil error-on-nil)
    (replique/repl-by :repl-type repl-type)))

(defun replique/guess-project-root-dir ()
  (or (locate-dominating-file default-directory "project.clj")
      default-directory))

(defun replique/replique-root-dir ()
  (-> (locate-library "replique")
      file-name-directory))




;; Auto completion

(defun replique/auto-complete-session (prefix company-callback tooling-repl repl)
  (let* ((tooling-chan (-h/get tooling-repl :chan))
         (repl-type (-h/get repl :repl-type))
         (msg-type (cond ((equal :clj repl-type)
                          :clj-completion)
                         ((equal :cljs repl-type)
                          :cljs-completion)
                         (t (error "Invalid REPL type: %s" repl-type)))))
    (replique/send-tooling-msg
     tooling-repl
     (-h/hash-map :type msg-type
                  :context (replique/form-with-prefix)
                  :ns (symbol-name (-h/get repl :ns))
                  :prefix prefix))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (-h/get resp :error)))
           (if err
               (progn
                 (message (replique-edn/pr-str err))
                 (message "completion failed with prefix %s" prefix))
             (let* ((candidates (-h/get resp :candidates))
                    (candidates (mapcar (lambda (c) (-h/get c :candidate)) candidates)))
               (funcall company-callback candidates)))))))))

(defun replique/auto-complete* (prefix company-callback tooling-repl msg-type)
  (let ((tooling-chan (-h/get tooling-repl :chan)))
    (replique/send-tooling-msg
     tooling-repl
     (-h/hash-map :type msg-type
                  :context (replique/form-with-prefix)
                  :ns (clojure-find-ns)
                  :prefix prefix))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (when resp
         (let ((err (-h/get resp :error)))
           (if err
               (progn
                 (message (replique-edn/pr-str err))
                 (message "completion failed with prefix %s" prefix))
             (let* ((candidates (-h/get resp :candidates))
                    (candidates (mapcar (lambda (c) (-h/get c :candidate)) candidates)))
               (funcall company-callback candidates)))))))))

(defun replique/auto-complete-clj (prefix company-callback tooling-repl clj-repl)
  (replique/auto-complete* prefix company-callback tooling-repl :clj-completion))

(defun replique/auto-complete-cljs (prefix company-callback tooling-repl cljs-repl)
  (replique/auto-complete* prefix company-callback tooling-repl :cljs-completion))

(defun replique/auto-complete-lein (prefix company-callback tooling-repl)
  (funcall company-callback '()))

(defun replique/auto-complete-cljs-env (prefix company-callback tooling-repl)
  (funcall company-callback '()))

(defun replique/auto-complete (prefix company-callback)
  (replique/with-modes-dispatch
   (:leiningen . (-partial 'replique/auto-complete-lein prefix company-callback))
   (:cljs-env . (-partial 'replique/auto-complete-cljs-env prefix company-callback))
   (replique/mode . (-partial 'replique/auto-complete-session prefix company-callback))
   (clojure-mode . (-partial 'replique/auto-complete-clj prefix company-callback))
   (clojurescript-mode . (-partial 'replique/auto-complete-cljs prefix company-callback))))

(defun replique/eldoc-documentation-function (callback)
  (let* ((tooling-repl (replique/active-repl :tooling))
         (tooling-chan (-h/get tooling-repl :chan)))
    (when tooling-chan
      (replique/send-tooling-msg
       tooling-repl
       (-h/hash-map :type :repliquedoc
                    :context (replique/form-with-prefix)
                    :ns (clojure-find-ns)
                    :symbol (symbol-name (symbol-at-point))))
      (replique-async/<!
       tooling-chan
       (lambda (resp)
         (when resp
           (let ((err (-h/get resp :error)))
             (if err
                 (progn
                   (message (replique-edn/pr-str err))
                   (message "eldoc failed"))
               (funcall callback (-h/get resp :doc))))))))))

(defun replique/in-ns-clj (ns-name tooling-repl clj-repl)
  (replique/send-input-from-source-clj-cljs
   (format "(clojure.core/in-ns '%s)" ns-name)
   'replique/display-eval-result
   tooling-repl clj-repl))

(defun replique/in-ns-cljs (ns-name tooling-repl cljs-repl)
  (replique/send-input-from-source-clj-cljs
   (format "(ewen.replique.cljs-env.macros/cljs-in-ns '%s)" ns-name)
   'replique/display-eval-result
   tooling-repl cljs-repl))

(defun replique/in-ns-cljc (ns-name tooling-repl clj-repl cljs-repl)
  (replique/send-input-from-source-cljc
   (format "(clojure.core/in-ns '%s)" ns-name)
   (format "(ewen.replique.cljs-env.macros/cljs-in-ns '%s)" ns-name)
   'replique/display-eval-result
   'replique/display-eval-results
   tooling-repl clj-repl cljs-repl))

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
   ((equal command 'candidates)
    `(:async . ,(-partial 'replique/auto-complete arg)))))

(defun replique/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique/comint-send-input (&optional no-read-eval-chan)
  (interactive)
  (let* ((buff (current-buffer))
         (repl (replique/repl-by :buffer buff))
         (eval-chan (-h/get repl :eval-chan))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input)
               ;; when replique/comint-send-input is called programmatically, the channel is
               ;; already handled elsewhere
               (when (null no-read-eval-chan)
                 (replique-async/<!
                  eval-chan
                  (lambda (msg)
                    (when msg
                      (replique/display-eval-result msg buff))))))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;; Point is before the prompt. Do nothing.
              (t nil))))))

(defun replique/format-eval-message (msg)
  (if (-h/get msg :error)
      (let ((value (-h/get msg :value))
            (repl-type (-h/get msg :repl-type))
            (ns (-h/get msg :ns)))
        (if (s-starts-with-p "Error:" value)
            (format "(%s) %s=> %s"
                    (replique/keyword-to-string repl-type) ns value)
          (format "(%s) %s=> Error: %s"
                  (replique/keyword-to-string repl-type) ns value)))
    (let ((result (-h/get msg :result))
          (repl-type (-h/get msg :repl-type))
          (ns (-h/get msg :ns)))
      (format "(%s) %s=> %s"
              (replique/keyword-to-string repl-type) ns result))))

(defun replique/display-eval-result (msg buff)
  (when (not (-contains? (replique/visible-buffers) buff))
    (replique/message-nolog
     (replique/format-eval-message msg))))

(defun replique/display-eval-results (msg1 msg2 clj-buff cljs-buff)
  (let ((visible-buffers (replique/visible-buffers))
        (clj-msg (-first (lambda (msg)
                           (equal :clj (-h/get msg :repl-type)))
                         (list msg1 msg2)))
        (cljs-msg (-first (lambda (msg)
                            (equal :cljs (-h/get msg :repl-type)))
                          (list msg1 msg2))))
    (cond ((and (not (-contains? visible-buffers clj-buff))
                (not (-contains? visible-buffers cljs-buff)))
           (replique/message-nolog
            "%s\n%s"
            (replique/format-eval-message clj-msg)
            (replique/format-eval-message cljs-msg)))
          ((not (-contains? visible-buffers clj-buff))
           (replique/message-nolog
            (replique/format-eval-message clj-msg)))
          ((not (-contains? visible-buffers cljs-buff))
           (replique/message-nolog
            (replique/format-eval-message cljs-msg))))))

(defun replique/format-load-file-message (msg)
  (if (-h/get msg :error)
      (let ((value (-h/get msg :value))
            (repl-type (-h/get msg :repl-type)))
        (if (s-starts-with-p "Error:" value)
            (format "(%s) load-file: %s"
                    (replique/keyword-to-string repl-type) value)
          (format "(%s) load-file: Error: %s"
                  (replique/keyword-to-string repl-type) value)))
    (let ((result (-h/get msg :result))
          (repl-type (-h/get msg :repl-type)))
      (format "(%s) load-file: %s"
              (replique/keyword-to-string repl-type) result))))

(defun replique/display-load-file-result (msg buff)
  (when (not (-contains? (replique/visible-buffers) buff))
    (replique/message-nolog
     (replique/format-load-file-message msg))))

(defun replique/display-load-file-results (msg1 msg2 clj-buff cljs-buff)
  (let ((visible-buffers (replique/visible-buffers))
        (clj-msg (-first (lambda (msg)
                           (equal :clj (-h/get msg :repl-type)))
                         (list msg1 msg2)))
        (cljs-msg (-first (lambda (msg)
                            (equal :cljs (-h/get msg :repl-type)))
                          (list msg1 msg2))))
    (cond ((and (not (-contains? visible-buffers clj-buff))
                (not (-contains? visible-buffers cljs-buff)))
           (replique/message-nolog
            "%s\n%s"
            (replique/format-load-file-message clj-msg)
            (replique/format-load-file-message cljs-msg)))
          ((not (-contains? visible-buffers clj-buff))
           (replique/message-nolog
            (replique/format-load-file-message clj-msg)))
          ((not (-contains? visible-buffers cljs-buff))
           (replique/message-nolog
            (replique/format-load-file-message cljs-msg))))))

(defun replique/file-in-root-dir (tooling-repl file-name)
  (and file-name
       (equal (-h/get tooling-repl :directory)
              (file-name-directory file-name))))

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
                        (if ,clj-buff-sym
                            (funcall ,f ,props-sym ,clj-repl-sym)
                          (user-error "No active Clojure REPL"))))
                     ;; No active clojure REPL is required
                     ;; For example, it is possible to load a clojure file
                     ;; when only a clojurescript REPL is active because of
                     ;; macro reloading
                     ((equal 'clojure-mode* m)
                      `((equal 'clojure-mode major-mode)
                        (if (or ,clj-buff-sym ,cljs-buff-sym)
                            (funcall ,f
                                     ,props-sym
                                     ,clj-repl-sym ,cljs-repl-sym)
                          (user-error "No active Clojure or Clojurescript REPL"))))
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
                     ((equal 'scss-mode m)
                      `((equal 'scss-mode major-mode)
                        (if ,cljs-buff-sym
                            (funcall ,f ,props-sym ,cljs-repl-sym)
                          (user-error "No active Clojurescript REPL"))))
                     ((equal 'replique/mode m)
                      `((equal 'replique/mode major-mode)
                        (funcall ,f ,props-sym
                                 (replique/repl-by :buffer (current-buffer)))))
                     ((equal :leiningen m)
                      `((and (equal 'clojure-mode major-mode)
                             (equal "project.clj" (file-name-nondirectory (buffer-file-name)))
                             (replique/file-in-root-dir ,props-sym (buffer-file-name)))
                        (if (or ,clj-buff-sym ,cljs-buff-sym)
                            (funcall ,f ,props-sym)
                          (user-error "No active Clojure or Clojurescript REPL"))))
                     ((equal :cljs-env m)
                      `((and (equal 'clojure-mode major-mode)
                             (equal ".replique-cljs-repl-env.clj"
                                    (file-name-nondirectory (buffer-file-name)))
                             (replique/file-in-root-dir ,props-sym (buffer-file-name)))
                        (if (or ,clj-buff-sym ,cljs-buff-sym)
                            (funcall ,f ,props-sym)
                          (user-error "No active Clojure or Clojurescript REPL")))))))
           modes-alist))
         (dispatch-code (append dispatch-code
                                '((t (user-error
                                      "Cannot eval from major mode: %s"
                                      major-mode))))))
    `(let* ((,props-sym (replique/active-repl :tooling t))
            (,clj-repl-sym (replique/active-repl :clj))
            (,cljs-repl-sym (replique/active-repl :cljs))
            (,clj-buff-sym (-h/get ,clj-repl-sym :buffer))
            (,cljs-buff-sym (-h/get ,cljs-repl-sym :buffer)))
       (cond ,@dispatch-code))))

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
         (replique/comint-send-input t)
         (goto-char (process-mark process))
         (insert old-input))))

(defun replique/send-input-from-source-clj-cljs
    (input callback props repl)
  (let ((buff (-h/get repl :buffer))
        (eval-chan (-h/get repl :eval-chan)))
    (with-current-buffer buff
      (replique/comint-send-input-from-source input))
    (replique-async/<!
     eval-chan
     (lambda (msg)
       (when msg
         (funcall callback msg buff))))))

(defun replique/send-input-from-source-cljc
    (input-clj input-cljs display-result-fn display-results-fn
               props clj-repl cljs-repl)
  (let ((clj-buff (-h/get clj-repl :buffer))
        (clj-eval-chan (-h/get clj-repl :eval-chan))
        (cljs-buff (-h/get  cljs-repl :buffer))
        (cljs-eval-chan (-h/get cljs-repl :eval-chan)))
    (when clj-buff
      (with-current-buffer clj-buff
        (replique/comint-send-input-from-source input-clj)))
    (when cljs-buff
      (with-current-buffer cljs-buff
        (replique/comint-send-input-from-source input-cljs)))
    (cond ((and clj-buff cljs-buff)
           (let ((chan (replique-async/chan)))
             (replique-async/<!
              clj-eval-chan
              (lambda (msg)
                (when msg
                  (replique-async/put! chan msg))))
             (replique-async/<!
              cljs-eval-chan
              (lambda (msg)
                (when msg
                  (replique-async/put! chan msg))))
             (replique-async/<!
              chan
              (lambda (msg1)
                (replique-async/<!
                 chan
                 (lambda (msg2)
                   (funcall display-results-fn
                            msg1 msg2 clj-buff cljs-buff))))
              t)))
          (clj-buff
           (replique-async/<!
                     clj-eval-chan
                     (lambda (msg)
                       (when msg
                         (funcall display-result-fn msg clj-buff)))
                     t))
          (cljs-buff
           (replique-async/<!
                     cljs-eval-chan
                     (lambda (msg)
                       (when msg
                         (funcall display-result-fn msg cljs-buff)))
                     t)))))

(defun replique/send-input-from-source-dispatch (input)
  (replique/with-modes-dispatch
   (clojure-mode . (-partial
                    'replique/send-input-from-source-clj-cljs
                    input 'replique/display-eval-result))
   (clojurescript-mode . (-partial
                          'replique/send-input-from-source-clj-cljs
                          input 'replique/display-eval-result))
   (clojurec-mode . (-partial
                     'replique/send-input-from-source-cljc
                     input input
                     'replique/display-eval-result
                     'replique/display-eval-results))))

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

(defun replique/load-file-clj (file-path props clj-repl cljs-repl)
  (if clj-repl
      (replique/send-input-from-source-clj-cljs
       (format "(clojure.core/load-file \"%s\")" file-path)
       'replique/display-load-file-result
       props clj-repl)
    (replique/send-input-from-source-clj-cljs
     (format "(ewen.replique.cljs-env.macros/load-file :clj \"%s\")"
             file-path)
     'replique/display-load-file-result
     props cljs-repl)))

(defun replique/load-file-cljs (file-path props cljs-repl)
  (replique/send-input-from-source-clj-cljs
   (format "(ewen.replique.cljs-env.macros/load-file \"%s\")"
           file-path)
   'replique/display-load-file-result
   props cljs-repl))

(defun replique/load-file-cljc (file-path props clj-repl cljs-repl)
  (replique/send-input-from-source-cljc
   (format "(clojure.core/load-file \"%s\")" file-path)
   (format "(ewen.replique.cljs-env.macros/load-file \"%s\")" file-path)
   'replique/display-load-file-result
   'replique/display-load-file-results
   props clj-repl cljs-repl))

(defun replique/restart-cljs-repls (tooling-repl)
  (let* ((directory (-h/get tooling-repl :directory))
         (cljs-repls (replique/repls-by :directory directory
                                        :repl-type :cljs)))
    (mapcar (lambda (cljs-repl)
              (replique/send-input-from-source-clj-cljs
               ":cljs/quit"
               (lambda (msg buff)
                 (replique/send-input-from-source-clj-cljs
                  "(ewen.replique.server-cljs/cljs-repl)"
                  (lambda (msg buff)
                    nil)
                  tooling-repl
                  cljs-repl))
               tooling-repl
               cljs-repl))
            cljs-repls)))

(defun replique/load-cljs-repl-env (tooling-repl &optional callback)
  (message "Loading Clojurescript REPL environement...")
  (let* ((tooling-chan (-h/get tooling-repl :chan))
         (cljs-env-opts (buffer-string)))
    (replique/send-tooling-msg
     tooling-repl
     (-h/hash-map :type :set-cljs-env
                  :cljs-env-opts cljs-env-opts))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (cond ((-h/get resp :error)
              (message (replique-edn/pr-str (-h/get resp :error)))
              (message "Loading Clojurescript REPL environement: failed"))
             ((-h/get resp :invalid)
              (message (s-replace-all '(("%" . "%%")) (-h/get resp :invalid))))
             (t
              (replique/restart-cljs-repls tooling-repl)
              (message "Loading Clojurescript REPL environement: done")))
       (when callback
         (funcall callback resp))))))

(defun replique/load-file ()
  "Load a file in a replique REPL"
  (interactive)
  (let ((file-path (buffer-file-name)))
    (comint-check-source file-path)
    (replique/with-modes-dispatch
     (:cljs-env . 'replique/load-cljs-repl-env)
     (clojure-mode* . (-partial 'replique/load-file-clj file-path))
     (clojurescript-mode . (-partial 'replique/load-file-cljs file-path))
     (clojurec-mode . (-partial 'replique/load-file-cljc file-path))
     (css-mode . (-partial 'replique/load-css file-path))
     (scss-mode . (-partial 'replique/load-scss file-path)))))

(defun replique/switch-active-repl (repl-buff-name)
  "Switch the currently active REPL"
  (interactive
   (let* ((repls (-filter (lambda (repl)
                            (let ((repl-type (-h/get repl :repl-type)))
                              (not (equal :tooling repl-type))))
                          replique/repls))
          (repl-names (mapcar (lambda (repl)
                                (buffer-name (-h/get repl :buffer)))
                              repls)))
     (when (null repls)
       (user-error "No started REPL"))
     (list (ido-completing-read "Switch to REPL: " repl-names nil t))))
  (let* ((buffer (get-buffer repl-buff-name))
         (repl (replique/repl-by :buffer buffer))
         (repl-type (-h/get repl :repl-type))
         (prev-active-repl (replique/repl-by :repl-type repl-type))
         (prev-active-buffer (-h/get prev-active-repl :buffer))
         (prev-active-windows (get-buffer-window-list prev-active-buffer)))
    (setq replique/repls (delete repl replique/repls))
    (setq replique/repls (push repl replique/repls))
    (mapcar (lambda (window)
              (set-window-buffer window buffer))
            prev-active-windows)))

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
  (setq comint-prompt-regexp replique/prompt)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (set-syntax-table clojure-mode-syntax-table)
  (add-to-list 'company-backends 'replique/company-backend))

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
  (setq-local eldoc-documentation-function 'replique/eldoc-documentation-function))

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

(defun replique/raw-command (port)
  `("java" "-cp" ,(replique/raw-command-classpath) "clojure.main" "-m" "ewen.replique.main"
    ,(format "{:type :clj :port %s}" (number-to-string port))))

(defun replique/lein-command (port)
  `(,(or replique/lein-script "lein") "update-in" ":source-paths" "conj"
    ,(format "\"%sclj/src\"" (replique/replique-root-dir))
    "--" "run" "-m" "ewen.replique.main/-main"
    ,(format "{:type :clj :port %s}" (number-to-string port))))

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
  (let ((tooling-network-proc (-h/get tooling-repl :network-proc)))
    (process-send-string
     tooling-network-proc
     (format "(ewen.replique.server/tooling-msg-handle %s)\n"
             (replique-edn/pr-str msg)))))

;; Note: a tooling repl may not be closed when there is a pending non daemon thread
;; We don't want to save REPLs when closing the process, as opposed to closing a single REPL
(defun replique/close-tooling-repl (repl &optional dont-save-repls?)
  (let ((tooling-proc (-h/get repl :proc))
        (tooling-chan (-h/get repl :chan)))
    (replique-async/close! tooling-chan)
    (when (process-live-p tooling-proc)
      (delete-process tooling-proc))
    (setq replique/repls (delete repl replique/repls))))

(defun replique/close-repl (repl-props &optional dont-save-repls?)
  (let* ((buffer (-h/get repl-props :buffer))
         (host (-h/get repl-props :host))
         (port (-h/get repl-props :port))
         (eval-chan (-h/get repl-props :eval-chan))
         (directory (-h/get repl-props :directory))
         (proc (get-buffer-process buffer)))
    (when proc
      (set-process-sentinel proc nil)
      (delete-process proc))
    (kill-buffer buffer)
    (replique-async/close! eval-chan)
    (setq replique/repls (delete repl-props replique/repls))
    (when (not dont-save-repls?)
      (replique/save-repls directory))
    ;; If the only repl left is a tooling repl, then close it
    (let ((other-repls (replique/repls-by :host host :port port)))
      (when (-all? (lambda (repl)
                     (equal :tooling (-h/get repl :repl-type)))
                   other-repls)
        (mapcar (lambda (tooling-repl)
                  (let ((tooling-chan (-h/get tooling-repl :chan)))
                    (replique/send-tooling-msg
                     tooling-repl (-h/hash-map :type :shutdown))
                    (replique-async/<!
                     tooling-chan
                     (lambda (msg)
                       (when (and msg (-h/get msg :error))
                         (error "Error while shutting down the REPL: %s"
                                (replique-edn/pr-str (-h/get msg :error))))))))
                other-repls)))))

(defun replique/close-repls (host port &optional dont-save-repls?)
  (let* ((repls (replique/repls-by :host host :port port))
         (not-tooling-repls (-filter (lambda (repl)
                                       (not (equal :tooling (-h/get repl :repl-type))))
                                     repls))
         (tooling-repls (-filter (lambda (repl)
                                   (equal :tooling (-h/get repl :repl-type)))
                                 repls)))
    (mapcar (lambda (repl)
              (replique/close-repl repl dont-save-repls?))
            not-tooling-repls)
    (mapcar (lambda (repl)
              (replique/close-tooling-repl repl))
            tooling-repls)))

(defun replique/on-tooling-repl-close
    (tooling-chan-src host port process event)
  (cond ((string= "deleted\n" event)
         (replique/close-repls host port))
        ((string= "connection broken by remote peer\n" event)
         (replique/close-repls host port))
        (t nil)))

;; java -cp "/Users/egr/bin/clojure-1.8.0.jar:/Users/egr/bin/cljs-1.9.216.jar:/Users/egr/replique.el/clj/src" clojure.main -m ewen.replique.main "{:type :clj :port 0}"

(defun replique/dispatch-repl-cmd (directory port)
  (if (replique/is-lein-project directory) 
      (replique/lein-command port)
    (replique/raw-command port)))

(defun replique/is-in-exec-path (file absolute?)
  (-> (-mapcat
       (lambda (dir)
         (directory-files dir absolute?))
       exec-path)
      (-contains? file)))

(defun replique/repl-cmd-pre-cond (directory)
  (let ((is-lein-project (replique/is-lein-project directory)))
    (cond ((and is-lein-project
                replique/lein-script
                (not (replique/is-in-exec-path replique/lein-script t)))
           (message "Error while starting the REPL. %s could not be find in exec-path. Please update replique/lein-script"
                    replique/lein-script)
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
               (cond
                (filtered? (progn
                             (replique-async/put! filtered-chan msg)
                             (replique/skip-repl-starting-output* proc-chan filtered-chan t)))
                ((equal "Starting Clojure REPL..." (s-trim msg))
                 (replique/skip-repl-starting-output* proc-chan filtered-chan t))
                (t (progn
                     (message msg)
                     (replique/skip-repl-starting-output* proc-chan filtered-chan)))))))

(defun replique/skip-repl-starting-output (proc-chan)
  (let ((filtered-chan (replique-async/chan)))
    (replique/skip-repl-starting-output* proc-chan filtered-chan)
    filtered-chan))

(defun replique/make-tooling-repl (host port directory out-chan)
  (let* ((default-directory directory)
         (random-port? (equal port 0))
         (repl-cmd (replique/dispatch-repl-cmd directory port))
         (proc (apply 'start-process directory
                      nil
                      (car repl-cmd) (cdr repl-cmd)))
         (proc-chan (replique/skip-repl-starting-output (replique/process-filter-chan proc)))
         (chan (replique/read-chan
                proc-chan proc
                (lambda (proc-out-s)
                  (error "Error while starting the REPL: %s" proc-out-s)))))
    (replique-async/<!
     chan (lambda (repl-infos)
            (set-process-filter proc (lambda (proc string) nil))
            (if (-h/get repl-infos :error)
                (message "Error while starting the REPL: %s"
                         (replique-edn/pr-str (-h/get repl-infos :error)))
              (let* ((host (-h/get repl-infos :host))
                     (port (-h/get repl-infos :port))
                     (directory (replique/normalize-directory-name
                                 (-h/get repl-infos :directory)))
                     (network-proc (open-network-stream directory nil host port))
                     (tooling-chan (replique/process-filter-chan network-proc)))
                (set-process-sentinel
                 network-proc (-partial 'replique/on-tooling-repl-close chan host port))
                ;; Discard the prompt
                (replique-async/<!
                 tooling-chan (lambda (x)
                                ;; No need to wait for the return value of shared-tooling-repl
                                ;; since it does not print anything
                                (process-send-string
                                 network-proc
                                 "(ewen.replique.server/shared-tooling-repl)\n")
                                (let* ((tooling-chan (-> tooling-chan
                                                         (replique/read-chan network-proc)
                                                         replique/dispatch-eval-msg))
                                       (tooling-repl (-h/hash-map
                                                      :directory directory
                                                      :repl-type :tooling
                                                      :proc proc
                                                      :network-proc network-proc
                                                      :host host
                                                      :port port
                                                      :random-port? random-port?
                                                      :chan tooling-chan))
                                       (cljs-env-file (concat
                                                       directory
                                                       ".replique-cljs-repl-env.clj")))
                                  (if (file-exists-p cljs-env-file)
                                      (with-current-buffer (find-file-noselect cljs-env-file)
                                        (replique/load-cljs-repl-env
                                         tooling-repl (lambda (resp)
                                                        (push tooling-repl replique/repls)
                                                        (replique-async/put!
                                                         out-chan tooling-repl))))
                                    (progn (push tooling-repl replique/repls)
                                           (replique-async/put!
                                            out-chan tooling-repl))))))))))))

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
          (directories (mapcar (lambda (repl) (-h/get repl :directory)) tooling-repls)))
     (list (ido-completing-read "Close process: " directories nil t))))
  (let* ((tooling-repl (replique/repl-by :repl-type :tooling :directory directory))
         (host (-h/get tooling-repl :host))
         (port (-h/get tooling-repl :port)))
    (replique/close-repls host port t)))

(defun replique/make-repl (buffer-name directory host port &optional repl-type)
  (let* ((buff (get-buffer-create buffer-name))
         (buff (make-comint-in-buffer buffer-name buff `(,host . ,port)))
         (proc (get-buffer-process buff))
         (chan-src (replique/process-filter-chan proc))
         (repl-cmd (format "(ewen.replique.server/repl :clj)\n")))
    (set-process-sentinel proc (-partial 'replique/on-repl-close host port buff))
    ;; Discard the prompt
    (replique-async/<!
     chan-src
     (lambda (x)
       (let ((chan (replique/read-chan chan-src proc)))
         (process-send-string proc "(ewen.replique.server/tooling-repl)\n")
         ;; Get the session number
         (process-send-string proc "clojure.core.server/*session*\n")
         (replique-async/<!
          chan
          (lambda (resp)
            (let ((session (-h/get resp :client))
                  (eval-chan (replique-async/chan)))
              ;; Reset process filter to the default one
              (set-process-filter proc 'comint-output-filter)
              (set-buffer buff)
              (replique/mode)
              (process-send-string proc repl-cmd)
              (let ((repl (-h/hash-map :directory directory
                                       :host host
                                       :port port
                                       :repl-type (or repl-type :clj)
                                       :session session
                                       :ns 'user
                                       :buffer buff
                                       :eval-chan eval-chan)))
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
                  ;; Third parameter is nil because the function does not use the tooling repl
                  ;; anyway
                  (replique/send-input-from-source-clj-cljs
                   "(ewen.replique.server/cljs-repl)" (lambda (msg buff) nil) nil repl))
                (display-buffer buff))))))))))

(defun replique/clj-buff-name (directory repl-type)
  (let ((repl-type-string (replique/keyword-to-string repl-type)))
    (generate-new-buffer-name
     (format "*replique*%s*%s*"
             (file-name-nondirectory (directory-file-name directory))
             repl-type-string))))

(defun replique/save-repls (directory)
  (let ((repls (replique/repls-by :directory directory)))
    (if (or
         (null repls)
         (and (equal 1 (length repls))
              (equal :tooling (-h/get (car repls) :repl-type))))
        (when (file-exists-p (concat directory ".replique-repls"))
          (delete-file (concat directory ".replique-repls")))
      (with-temp-file (concat directory ".replique-repls")
        (let* ((repls (mapcar (lambda (repl)
                                (let ((host (-h/get repl :host))
                                      (port (-h/get repl :port))
                                      (repl-type (-h/get repl :repl-type))
                                      (random-port? (-h/get repl :random-port?))
                                      (buffer (-h/get repl :buffer)))
                                  (if (equal repl-type :tooling)
                                      (-h/hash-map :host host :port port
                                                   :random-port? random-port?
                                                   :repl-type repl-type)
                                    (-h/hash-map :repl-type repl-type
                                                 :buffer-name (buffer-name buffer)))))
                              replique/repls))
               (repls-string (with-output-to-string (pp repls))))
          (insert repls-string))))))

(defun replique/validate-saved-repls (validated saved-repls)
  (if (null saved-repls)
      validated
    (when (hash-table-p (car saved-repls))
      (let* ((saved-repl (car saved-repls))
             (repl-type (-h/get saved-repl :repl-type))
             (host (-h/get saved-repl :host))
             (port (-h/get saved-repl :port))
             (random-port? (-h/get saved-repl :random-port?))
             (buffer-name (-h/get saved-repl :buffer-name)))
        (cond ((and (equal repl-type :tooling)
                    (not (null host)) (stringp host)
                    (replique/is-valid-port-nb? port))
               (replique/validate-saved-repls
                (cons saved-repl validated) (cdr saved-repls)))
              ((and (or (equal :clj repl-type) (equal :cljs repl-type))
                    (not (null buffer-name)) (stringp buffer-name))
               (replique/validate-saved-repls
                (cons saved-repl validated) (cdr saved-repls)))
              ;; This is an error, return nil
              (t nil))))))

;; Returns nil in case of an error
;; Return 0 as port number in case of a random port
(defun replique/read-saved-repls (directory)
  (when (file-exists-p (concat directory ".replique-repls"))
    (let* ((saved-repls (with-temp-buffer
                          (insert-file-contents (concat directory ".replique-repls"))
                          (buffer-string)))
           (saved-repls (condition-case err
                            (read saved-repls)
                          (error (message "Error while reading saved REPLs: %s"
                                          (error-message-string err))
                                 nil))))
      (replique/validate-saved-repls nil saved-repls))))

(defun replique/normalize-directory-name (directory)
  (file-name-as-directory (file-truename directory)))

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
               (saved-host (-h/get saved-tooling-repl :host))
               (saved-port (-h/get saved-tooling-repl :port))
               (random-port? (-h/get saved-tooling-repl :random-port?))
               (host (or host saved-host "127.0.0.1"))
               (port (or port (if random-port? 0 saved-port) (read-number "Port number: " 0))))
          (if (not (replique/is-valid-port-nb? port))
              (message "Invalid port number: %d" port)
            (replique/make-tooling-repl host port directory tooling-repl-chan))))
      (replique-async/<!
       tooling-repl-chan
       (-> (lambda (saved-repls repl)
             (let ((directory (-h/get repl :directory))
                   (host (-h/get repl :host))
                   (port (-h/get repl :port))
                   (tooling-chan (-h/get repl :chan)))
               (if saved-repls
                   (mapcar (lambda (repl)
                             (let ((repl-type (-h/get repl :repl-type))
                                   (buffer-name (-h/get repl :buffer-name)))
                               (when (not (equal repl-type :tooling))
                                 ;; make-repl expects buffer-name to be the name of no already
                                 ;; existing buffer
                                 (replique/make-repl (generate-new-buffer-name buffer-name)
                                                     directory host port repl-type))))
                           saved-repls)
                 (let* ((buff-name (replique/clj-buff-name directory :clj)))
                   (replique/make-repl buff-name directory host port)))))
           (-partial saved-repls))))))

(defun replique/on-repl-type-change (repl new-repl-type)
  (let* ((directory (-h/get repl :directory))
         (repl-type (-h/get repl :repl-type))
         (repl-type-s (replique/keyword-to-string repl-type))
         (new-repl-type-s (replique/keyword-to-string new-repl-type))
         (repl-buffer (-h/get repl :buffer)))
    (when (not (equal repl-type new-repl-type))
      (when (string-match-p (format "\\*%s\\*\\(<[0-9]+>\\)?$" repl-type-s)
                            (buffer-name repl-buffer))
        (let* ((new-buffer-name (replace-regexp-in-string
                                 (format "\\*%s\\*\\(<[0-9]+>\\)?$" repl-type-s)
                                 (format "*%s*" new-repl-type-s)
                                 (buffer-name repl-buffer))))
          (with-current-buffer repl-buffer
            (rename-buffer (generate-new-buffer-name new-buffer-name)))))
      (replique/update-repl repl (-h/assoc repl :repl-type new-repl-type))
      (replique/save-repls directory))))

(defun replique/dispatch-eval-msg* (in-chan out-chan)
  (replique-async/<!
   in-chan
   (lambda (msg)
     (cond (;; Global error (uncaught exception)
            (and 
             (-h/get msg :error)
             (-h/get msg :thread))
            (message
             (format "%s - Thread: %s - Exception: %s"
                     (propertize "Uncaught exception" 'face '(:foreground "red"))
                     (-h/get msg :thread)
                     (replique-edn/pr-str (-h/get msg :value))))
            (replique/dispatch-eval-msg* in-chan out-chan))
           ((equal :eval (-h/get msg :type))
            (let ((repl (replique/repl-by
                         :session (-h/get (-h/get msg :session) :client))))
              (when repl
                (replique/on-repl-type-change repl (-h/get msg :repl-type))
                (replique/update-repl repl (-h/assoc repl :ns (-h/get msg :ns)))
                (replique-async/put! (-h/get repl :eval-chan) msg)))
            (replique/dispatch-eval-msg* in-chan out-chan))
           (t (replique-async/put! out-chan msg)
              (replique/dispatch-eval-msg* in-chan out-chan))))))

(defun replique/dispatch-eval-msg (in-chan)
  (let ((out-chan (replique-async/chan)))
    (replique/dispatch-eval-msg* in-chan out-chan)
    out-chan))

(defun replique/read-chan*
    (chan-in chan-out buffer proc state &optional error-handler)
  (replique-async/<!
   chan-in
   (lambda (string)
     (condition-case err
         (progn
           (when (null buffer)
             (setq buffer (->> (process-name proc)
                               (format "*%s*")
                               (concat " ")
                               generate-new-buffer)))
           (with-current-buffer buffer
             (let ((continue t)
                   (insert-string? t))
               (while continue
                 (let ((parse-start (point)))
                   (when insert-string?
                     (insert string)
                     (setq insert-string? nil))
                   (let ((new-state (parse-partial-sexp parse-start (point-max) 0 nil state))
                         (parse-end (point)))
                     (if (not (= 0 (car new-state)))
                         (progn
                           (setq state new-state)
                           (setq continue nil))
                       (if (= parse-start parse-end)
                           (progn
                             (setq continue nil)
                             (when buffer
                               (kill-buffer buffer)
                               (setq buffer nil)))
                         (progn
                           (goto-char parse-start)
                           (let* ((s (delete-and-extract-region (point-min) parse-end))
                                  (s (s-trim s))
                                  (o (when (and s (not (equal "" s))) (read s))))
                             (setq state nil)
                             (when o
                               (replique-async/put! chan-out o)))))))))))
           (replique/read-chan* chan-in chan-out buffer proc state error-handler))
       (error
        (if error-handler
            (funcall error-handler string)
          (error (error-message-string err))))))))

(defun replique/read-chan (chan-in proc &optional error-handler)
  (let ((chan-out (replique-async/chan))
        (state nil)
        (buffer nil))
    (replique/read-chan* chan-in chan-out buffer proc state error-handler)
    chan-out))

(provide 'replique2)

;; Choose active proc
;; Remove tooling messages REPL history, add load-file to main mode to load current ns
;; API namespace for public functions
;; Make starting a new REPl proc possible using symbolic links
;; Interactive function for opening .replique-cljs-env.clj
;; Auto complete, jump to definition
;; Epresent
;; css, garden, js
;; sourcepath, classpath live reload
;; var explorer
;; Check reflection *warn-on-reflection*
;; compliment documentation, metadata
;; compliment invalidate memoized on classpath update
;; compliment for cljc
;; compliment keywords cljs -> missing :require ... ?
;; remove emacs auto save files 
;; check for nil when reading from chan because the chan can be closed
;; make print-length and print-level customizable
;; Check print-length/print-level for cljs
;; remove dash

;; replique.el ends here
