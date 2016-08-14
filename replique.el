;;; replique.el ---   -*- lexical-binding: t; -*-
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

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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

(defun replique/read-symbol (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (input (read-string prompt))
               (input (if (zerop (length input)) default input)))
          (if (zerop (length input)) nil (make-symbol input)))))

(defgroup replique nil
  ""
  :group 'clojure)

(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

(defvar replique/processes nil)

(defun replique/alist-to-map (alist)
  (let ((m (make-hash-table :test 'equal)))
    (mapcar (-lambda ((k . v))
              (puthash k v m))
            alist)
    m))

(defun replique/process-filter-chan (proc)
  (let ((chan (replique-async/chan)))
    (set-process-filter
     proc (lambda (proc string)
            (replique-async/put! chan string)))
    chan))

(defun replique/process-props-by (pred &optional error-on-nil)
  (let ((props (-first pred replique/processes)))
    (if (and (null props) error-on-nil)
        (user-error "No started REPL")
      props)))

(defun replique/processes-props-by (pred &optional error-on-nil)
  (let ((props (-filter pred replique/processes)))
    (if (and (null props) error-on-nil)
        (user-error "No started REPL")
      props)))

(defun replique/active-process-props (&optional error-on-nil)
  (replique/process-props-by
   (-lambda ((&alist :active active))
     active)
   error-on-nil))

(defun replique/process-props-by-host-port
    (host port &optional error-on-nil)
  (replique/process-props-by
   (-lambda ((&alist :host h :port p))
     (and (string= h host) (equal p port)))
   error-on-nil))

(defun replique/get-active-clj-repl (&optional error-on-nil)
  (->> (replique/active-process-props error-on-nil)
       (assoc :active-clj-repl) cdr))

(defun replique/get-active-cljs-repl (&optional error-on-nil)
  (->> (replique/active-process-props error-on-nil)
       (assoc :active-cljs-repl) cdr))

(defun replique/get-active-repl-by-type (type &optional error-on-nil)
  (let ((res (-first (lambda (repl)
                       (equal type (cdr (assoc :type repl))))
                     (list (replique/get-active-clj-repl error-on-nil)
                           (replique/get-active-cljs-repl error-on-nil)))))
    (when (and error-on-nil (null res))
      (error "Invalid type %s" type))
    res))

(defun replique/get-repl-by-session
    (repl-type session &optional error-on-nil)
  (let* ((props (replique/active-process-props error-on-nil))
         (repls (cond ((equal :clj repl-type)
                       (cdr (assoc :clj-repls props)))
                      ((equal :cljs repl-type)
                       (cdr (assoc :cljs-repls props)))
                      (t '()))))
    (-first (lambda (repl)
              (equal (cdr (assoc :session repl)) session))
            repls)))

(defun replique/get-repl-by-buffer
    (buffer &optional error-on-nil)
  (-some
   (lambda (props)
     (let* ((clj-repls (cdr (assoc :clj-repls props)))
            (cljs-repls (cdr (assoc :cljs-repls props))))
       (or (-first (lambda (repl)
                     (eq (cdr (assoc :buffer repl)) buffer))
                   clj-repls)
           (-first (lambda (repl)
                     (eq (cdr (assoc :buffer repl)) buffer))
                   cljs-repls))))
   replique/processes))

(defun replique/set-active-process
    (host port &optional display-msg)
  (interactive
   (let* ((buffer-descs (mapcar
                         (-lambda ((&alist :directory dir
                                     :host host
                                     :port port))
                           `(,(format "%s|%s:%s" dir host port)
                             . (,host ,port)))
                         replique/processes))
          (buffer-names (mapcar 'car buffer-descs))
          (choosen (ido-completing-read
                    "Set active buffer to: "
                    buffer-names nil t)))
     (append (cdr (-first
                   (lambda (x) (string= (car x) choosen))
                   buffer-descs))
             '(t))))
  (let ((active-proc nil))
    (mapcar
     (lambda (props)
       (-let (((&alist :host proc-host :port proc-port) props))
         (if (and (string= proc-host host)
                  (equal proc-port port))
             (progn (setcdr (assoc :active props) t)
                    (setq active-proc props))
           (setcdr (assoc :active props) nil))))
     replique/processes)
    (if (and display-msg active-proc)
        (-let (((&alist :directory dir :host host :port port) active-proc))
          (message "Active process switched to %s"
                   (format "%s|%s:%s" dir host port)))
      (message "Active process switched"))))

(defun replique/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique/comint-send-input (&optional no-read-eval-chan)
  (interactive)
  (let* ((buff (current-buffer))
         (repl (replique/get-repl-by-buffer buff))
         (eval-chan (cdr (assoc :eval-chan repl)))
         (proc (get-buffer-process buff)))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input)
               (when (null no-read-eval-chan)
                 (replique-async/<!
                  eval-chan
                  (lambda (msg)
                    (when msg
                      (replique/display-eval-result msg buff)))
                  t)))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;; Point is before the prompt. Do nothing.
              (t nil))))))

(defun comint-kill-input ()
  "Kill all text from last stuff output by interpreter to point."
  (interactive)
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (> (point) (marker-position pmark))
	(kill-region pmark (point)))))

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

(defun replique/keyword-to-string (k)
  (substring (symbol-name k) 1))

(defun replique/format-eval-message (msg)
  (if (gethash :error msg)
      (let ((value (gethash :value msg))
            (repl-type (gethash :repl-type msg))
            (ns (gethash :ns msg)))
        (if (s-starts-with-p "Error:" value)
            (format "(%s) %s=> %s"
                    (replique/keyword-to-string repl-type) ns value)
          (format "(%s) %s=> Error: %s"
                  (replique/keyword-to-string repl-type) ns value)))
    (let ((result (gethash :result msg))
          (repl-type (gethash :repl-type msg))
          (ns (gethash :ns msg)))
      (format "(%s) %s=> %s"
              (replique/keyword-to-string repl-type) ns result))))

(defun replique/display-eval-result (msg buff)
  (when (not (-contains? (replique/visible-buffers) buff))
    (replique/message-nolog
     (replique/format-eval-message msg))))

(defun replique/display-eval-results (msg1 msg2 clj-buff cljs-buff)
  (let ((visible-buffers (replique/visible-buffers))
        (clj-msg (-first (lambda (msg)
                           (equal :clj (gethash :repl-type msg)))
                         (list msg1 msg2)))
        (cljs-msg (-first (lambda (msg)
                            (equal :cljs (gethash :repl-type msg)))
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
  (if (gethash :error msg)
      (let ((value (gethash :value msg))
            (repl-type (gethash :repl-type msg)))
        (if (s-starts-with-p "Error:" value)
            (format "(%s) load-file: %s"
                    (replique/keyword-to-string repl-type) value)
          (format "(%s) load-file: Error: %s"
                  (replique/keyword-to-string repl-type) value)))
    (let ((result (gethash :result msg))
          (repl-type (gethash :repl-type msg)))
      (format "(%s) load-file: %s"
              (replique/keyword-to-string repl-type) result))))

(defun replique/display-load-file-result (msg buff)
  (when (not (-contains? (replique/visible-buffers) buff))
    (replique/message-nolog
     (replique/format-load-file-message msg))))

(defun replique/display-load-file-results (msg1 msg2 clj-buff cljs-buff)
  (let ((visible-buffers (replique/visible-buffers))
        (clj-msg (-first (lambda (msg)
                           (equal :clj (gethash :repl-type msg)))
                         (list msg1 msg2)))
        (cljs-msg (-first (lambda (msg)
                            (equal :cljs (gethash :repl-type msg)))
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
                          (user-error
                           "No active Clojure or Clojurescript REPL"))))
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
                          (user-error
                           "No active Clojure or Clojurescript REPL"))))
                     ((equal 'css-mode m)
                      `((equal 'css-mode major-mode)
                        (if ,cljs-buff-sym
                            (funcall ,f ,props-sym ,cljs-repl-sym)
                          (user-error
                           "No active Clojurescript REPL"))))
                     ((equal 'scss-mode m)
                      `((equal 'scss-mode major-mode)
                        (if ,cljs-buff-sym
                            (funcall ,f ,props-sym ,cljs-repl-sym)
                          (user-error
                           "No active Clojurescript REPL"))))
                     ((equal 'replique/mode m)
                      `((equal 'replique/mode major-mode)
                        (funcall ,f ,props-sym
                                 (replique/get-repl-by-buffer
                                  (current-buffer))))))))
           modes-alist))
         (dispatch-code (append dispatch-code
                                '((t (user-error
                                      "Cannot eval from major mode: %s"
                                      major-mode))))))
    `(let* ((,props-sym (replique/active-process-props t))
            (,clj-repl-sym (replique/get-active-clj-repl))
            (,cljs-repl-sym (replique/get-active-cljs-repl))
            (,clj-buff-sym (cdr (assoc :buffer ,clj-repl-sym)))
            (,cljs-buff-sym (cdr (assoc :buffer ,cljs-repl-sym))))
       (cond ,@dispatch-code))))

(defun replique/send-input-from-source-clj-cljs
    (input display-result-fn props repl)
  (let ((buff (cdr (assoc :buffer repl)))
        (eval-chan (cdr (assoc :eval-chan repl))))
    (with-current-buffer buff
      (replique/comint-send-input-from-source input))
    (replique-async/<!
     eval-chan
     (lambda (msg)
       (when msg
         (funcall display-result-fn msg buff)))
     t)))

(defun replique/send-input-from-source-cljc
    (input-clj input-cljs display-result-fn display-results-fn
               props clj-repl cljs-repl)
  (let ((clj-buff (cdr (assoc :buffer clj-repl)))
        (clj-eval-chan (cdr (assoc :eval-chan clj-repl)))
        (cljs-buff (cdr (assoc :buffer cljs-repl)))
        (cljs-eval-chan (cdr (assoc :eval-chan cljs-repl))))
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
                (replique-async/put! chan msg)))
             (replique-async/<!
              cljs-eval-chan
              (lambda (msg)
                (replique-async/put! chan msg)))
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

(defun replique/load-file-success (repl file-path res)
  (let ((buff (cdr (assoc :buffer repl))))
    (with-current-buffer buff
      (comint-output-filter
       (get-buffer-process (current-buffer)) res)
      (replique/comint-refresh-prompt))))

(defun replique/format-exception-via (via)
  (let ((type (gethash :type via))
        (message (gethash :message via))
        (at (gethash :at via)))
    (format "
  {:type %s
   :message %s
   :at %s}"
            (replique-edn/pr-str type)
            (replique-edn/pr-str message)
            (replique-edn/pr-str at))))

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

(defun replique/css-candidates (css-infos)
  (-let (((&hash :scheme scheme
                 :uri uri
                 :file-path file-path) css-infos))
    (cond ((string= "data" scheme)
           (concat "data-uri:" file-path))
          ((string= "http" scheme)
           uri)
          (t nil))))

(defun replique/sass-candidate (sass-infos)
  (-let (((&hash :scheme scheme
                 :file-path file-path
                 :css-file css-file) sass-infos))
    (cond ((string= "data" scheme)
           (concat "data-uri:" file-path))
          ((string= "http" scheme)
           css-file)
          (t nil))))

(defun replique/selected-saas-infos
    (selected-scheme target-file sass-infos-list)
  (if (string= "*new-data-uri*" target-file)
      nil
    (-find
     (-lambda ((&hash :scheme scheme
                :css-file css-file
                :main-source main-source
                :file-path file-path))
       (cond ((and (string= "http" selected-scheme)
                   (string= "http" scheme)
                   (string= target-file css-file))
              t)
             ((and (string= "data" selected-scheme)
                   (string= "data" scheme)
                   (string= target-file (concat "data-uri:" file-path)))
              t)
             (t nil)))
     sass-infos-list)))

(defun replique/uri-compare (url1 url2)
  (let* ((path1 (url-filename (url-generic-parse-url url1)))
         (path2 (url-filename (url-generic-parse-url url2)))
         (uri1 (-> (split-string path1 "/")
                   cdr))
         (uri2 (-> (split-string path2 "/")
                   cdr))
         (l1 (list-length uri1))
         (l2 (list-length uri2))
         (uri1 (-slice uri1 (- l1 (min l1 l2)) l1))
         (uri2 (-slice uri2 (- l2 (min l1 l2)) l2))
         (uri1 (apply 'concat uri1))
         (uri2 (apply 'concat uri2))
         (res (compare-strings uri1 0 (length uri1) uri2 0 (length uri2))))
    (if (equal t res)
        0 res)))

(defun replique/uri-sort-fn (reference uri1 uri2)
  (let ((diff1 (replique/uri-compare reference uri1))
        (diff2 (replique/uri-compare reference uri2)))
    (cond ((equal diff1 0) t)
          ((equal diff2 0) nil)
          ((>= (abs diff1) (abs diff2)) t)
          (t nil))))

(defun replique/list-css (props cljs-repl callback)
  (let ((tooling-chan (cdr (assoc :tooling-chan props))))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (replique/send-tooling-msg props `((:type . :list-css)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (error "List css failed"))
           (funcall callback (gethash :css-infos resp)))))
     t)))

(defun replique/load-css (file-path props cljs-repl)
  (replique/list-css
   props cljs-repl
   (lambda (css-infos)
     (let* ((tooling-chan (cdr (assoc :tooling-chan props)))
            (candidates (-map 'replique/css-candidates css-infos))
            (candidates (if (-contains?
                             candidates
                             (concat "data-uri:" file-path))
                            candidates
                          (cons "*new-data-uri*" candidates)))
            (candidates (-sort (-partial
                                'replique/uri-sort-fn
                                file-path)
                               candidates))
            (uri (ido-completing-read
                  "Reload css file: "
                  candidates
                  nil t))
            (scheme (cond ((string= "*new-data-uri*" uri)
                           "data")
                          ((s-starts-with? "data-uri:" uri)
                           "data")
                          (t "http")))
            (uri (cond ((string= "*new-data-uri*" uri)
                        file-path)
                       ((s-starts-with? "data-uri:" uri)
                        (s-chop-prefix "data-uri:" uri))
                       (t uri))))
       (replique/send-tooling-msg
        props
        `((:type . :load-css)
          (:file-path . ,file-path)
          (:uri . ,uri)
          (:scheme . ,scheme)))
       (replique-async/<!
        tooling-chan
        (lambda (resp)
          (let ((err (gethash :error resp)))
            (if err
                (progn
                  (message (replique-edn/pr-str err))
                  (message "load-css %s: failed" file-path))
              (message "load-css %s: done" file-path))))
        t)))))

(defun replique/list-sass (file-path props cljs-repl callback)
  (let ((tooling-chan (cdr (assoc :tooling-chan props))))
    (when (not cljs-repl)
      (user-error "No active Clojurescript REPL"))
    (replique/send-tooling-msg props `((:type . :list-sass)
                                        (:file-path . ,file-path)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (error "List sass failed"))
           (funcall callback (gethash :sass-infos resp)))))
     t)))

(defun replique/load-scss (file-path props cljs-repl)
  (replique/list-sass
   file-path props cljs-repl
   (lambda (sass-infos)
     (let* ((tooling-chan (cdr (assoc :tooling-chan props)))
            (candidates (-map 'replique/sass-candidate sass-infos))
            (candidates (if (-contains?
                             candidates (concat "data-uri:" file-path))
                            candidates
                          (append candidates '("*new-data-uri*"))))
            (target-file (ido-completing-read
                          "Compile scss to: "
                          candidates
                          nil t)))
       (if (null target-file)
           nil
         (let* ((scheme (cond ((string= "*new-data-uri*" target-file)
                               "data")
                              ((s-starts-with? "data-uri:" target-file)
                               "data")
                              (t "http")))
                (sass-infos (replique/selected-saas-infos
                             scheme target-file sass-infos))
                (uri (if (string= "*new-data-uri*" target-file)
                         nil
                       (gethash :uri sass-infos)))
                (main-source (cond ((string= "*new-data-uri*" target-file)
                                    file-path)
                                   ((string= "data" scheme)
                                    (gethash :file-path sass-infos))
                                   (t (gethash :main-source sass-infos))))
                (target-file (cond ((string= "*new-data-uri*" target-file)
                                    file-path)
                                   ((string= "data" scheme)
                                    (s-chop-prefix "data-uri:" target-file))
                                   (t target-file))))
           (replique/send-tooling-msg
            props
            `((:type . :load-scss)
              (:scheme . ,scheme)
              (:uri . ,uri)
              (:file-path . ,target-file)
              (:main-source . ,main-source)))
           (replique-async/<!
            tooling-chan
            (lambda (resp)
              (let ((err (gethash :error resp)))
                (if err
                    (progn
                      (message (replique-edn/pr-str err))
                      (message "load-scss %s: failed" file-path))
                  (message "load-scss %s: done" file-path))))
            t)))))))

(defun replique/load-file ()
  "Load a file in a replique REPL"
  (interactive)
  (let ((file-path (buffer-file-name)))
    (comint-check-source file-path)
    (replique/with-modes-dispatch
     (clojure-mode* . (-partial 'replique/load-file-clj file-path))
     (clojurescript-mode . (-partial 'replique/load-file-cljs file-path))
     (clojurec-mode . (-partial 'replique/load-file-cljc file-path))
     (css-mode . (-partial 'replique/load-css file-path))
     (scss-mode . (-partial 'replique/load-scss file-path)))))

(defun replique/in-ns-clj (ns-name props clj-repl)
  (replique/send-input-from-source-clj-cljs
   (format "(clojure.core/in-ns '%s)" ns-name)
   'replique/display-eval-result
   props clj-repl))

(defun replique/in-ns-cljs (ns-name props cljs-repl)
  (replique/send-input-from-source-clj-cljs
   (format "(ewen.replique.cljs-env.macros/cljs-in-ns '%s)" ns-name)
   'replique/display-eval-result
   props cljs-repl))

(defun replique/in-ns-cljc (ns-name props clj-repl cljs-repl)
  (replique/send-input-from-source-cljc
   (format "(clojure.core/in-ns '%s)" ns-name)
   (format "(ewen.replique.cljs-env.macros/cljs-in-ns '%s)" ns-name)
   'replique/display-eval-result
   'replique/display-eval-results
   props clj-repl cljs-repl))

(defun replique/in-ns (ns-name)
  (interactive (replique/symprompt "Set ns to" (clojure-find-ns)))
  (replique/with-modes-dispatch
   (clojure-mode . (-partial 'replique/in-ns-clj ns-name))
   (clojurescript-mode . (-partial 'replique/in-ns-cljs ns-name))
   (clojurec-mode . (-partial 'replique/in-ns-cljc ns-name)))) ;

(defun replique/skip-regexp-forward (regexp)
  (let ((data (match-data)))
    (when (looking-at regexp)
      (let ((match-length (-> (match-string 0)
                              length)))
        (forward-char match-length)
        (set-match-data data)
        (replique/skip-regexp-forward regexp)))
    (set-match-data data)))

(defun replique/skip-symbol-backward ()
  (skip-chars-backward (concat "^" clojure--sym-forbidden-1st-chars))
  (replique/skip-regexp-forward "#_\\|#\\|'"))

(defun replique/skip-symbol-forward ()
  (skip-chars-forward
   (concat "^" clojure--sym-forbidden-rest-chars)))

(defun replique/symbol-backward ()
  (save-excursion
    (let ((end (point)))
      (replique/skip-symbol-backward)
      (when (not (equal end (point)))
        (buffer-substring-no-properties (point) end)))))

(defun replique/symbol-at-point ()
  (save-excursion
    (replique/skip-symbol-backward)
    (let ((begin (point)))
      (replique/skip-symbol-forward)
      (when (not (equal begin (point)))
        (make-symbol
         (buffer-substring-no-properties begin (point)))))))

(defun replique/bounds-of-symbol-at-point ()
  (save-excursion
    (replique/skip-symbol-backward)
    (let ((begin (point)))
      (replique/skip-symbol-forward)
      (when (not (equal begin (point)))
        `(,begin . ,(point))))))

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
  (let ((bounds (replique/bounds-of-symbol-at-point)))
    (replique/temporary-invisible-change
     (if bounds
         (progn (delete-region (car bounds) (cdr bounds))
                (insert "__prefix__")
                (thing-at-point 'defun))
       nil))))

;; Execute in a temp buffer because company-mode expects the current buffer
;; to not change at all
(defun replique/form-with-prefix ()
  (let ((defun-bounds (bounds-of-thing-at-point 'defun)))
    (when defun-bounds
      (let* ((point-offset-backward (- (cdr defun-bounds) (point)))
             (defun-content (buffer-substring (car defun-bounds)
                                              (cdr defun-bounds))))
        (with-temp-buffer
          (insert defun-content)
          (backward-char point-offset-backward)
          (replique/form-with-prefix*))))))

(defun replique/pos-at-line-col (l c)
  (when l
    (save-excursion
      (goto-char (point-min))
      (forward-line (- l 1))
      (when c (move-to-column c))
      (point))))

(defun replique/goto-file (symbol meta)
  (let* ((file (when meta (gethash :file meta)))
         (line (when meta (gethash :line meta)))
         (column (when meta (gethash :column meta))))
    (if (fboundp 'xref-push-marker-stack)
        (xref-push-marker-stack)
      (with-no-warnings
        (ring-insert find-tag-marker-ring (point-marker))))
    (cond
     ((and file line column)
      (find-file file)
      (goto-char (replique/pos-at-line-col line column)))
     ((and file line)
      (find-file file)
      (goto-char (replique/pos-at-line-col line column)))
     (file
      (find-file file))
     (t
      (pop-tag-mark)
      (message "Don't know how to find '%s'" symbol)))))

(defun replique/jump-to-definition-clj (symbol props clj-repl)
  (let ((tooling-chan (cdr (assoc :tooling-chan props))))
    (replique/send-tooling-msg
     props
     `((:type . :clj-var-meta)
       (:context . ,(replique/form-with-prefix))
       (:ns . (quote ,(make-symbol (clojure-find-ns))))
       (:symbol . (quote ,symbol))
       (:keys . (quote (:file :line :column)))))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (message "jump-to-definition %s: failed" symbol))
           (replique/goto-file symbol (gethash :meta resp)))))
     t)))

(defun replique/jump-to-definition-cljs (symbol props cljs-repl)
  (let ((tooling-chan (cdr (assoc :tooling-chan props))))
    (replique/send-tooling-msg
     props
     `((:type . :cljs-var-meta)
       (:context . ,(replique/form-with-prefix))
       (:ns . (quote ,(make-symbol (clojure-find-ns))))
       (:symbol . (quote ,symbol))
       (:keys . (quote (:file :line :column)))))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (message "jump-to-definition %s: failed" symbol))
           (replique/goto-file symbol (gethash :meta resp)))))
     t)))

(defun replique/jump-to-definition (symbol)
  (interactive (let* ((sym-at-point (replique/symbol-at-point))
                      (at-point (and sym-at-point)))
                 (if at-point
                     (list at-point)
                   (replique/read-symbol "Symbol" nil))))
  (replique/with-modes-dispatch
   (clojure-mode . (-partial 'replique/jump-to-definition-clj symbol))
   (clojurescript-mode . (-partial 'replique/jump-to-definition-cljs
                                   symbol))))

;; Auto completion

(defun replique/auto-complete-session (prefix company-callback props repl)
  (let* ((tooling-chan (cdr (assoc :tooling-chan props)))
         (repl-type (cdr (assoc :type repl)))
         (msg-type (cond ((equal :clj repl-type)
                          :clj-completion)
                         ((equal :cljs repl-type)
                          :cljs-completion)
                         (t (error "Invalid REPL type: %s" repl-type)))))
    (replique/send-tooling-msg
     props
     `((:type . ,msg-type)
       (:context . ,(replique/form-with-prefix))
       (:ns . (quote ,(cdr (assoc :ns repl))))
       (:prefix . ,prefix)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (message "completion failed with prefix %s" prefix))
           (funcall company-callback (gethash :candidates resp)))))
     t)))

(defun replique/auto-complete* (prefix company-callback props msg-type)
  (let ((tooling-chan (cdr (assoc :tooling-chan props))))
    (replique/send-tooling-msg
     props
     `((:type . ,msg-type)
       (:context . ,(replique/form-with-prefix))
       (:ns . (quote ,(make-symbol (clojure-find-ns))))
       (:prefix . ,prefix)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let ((err (gethash :error resp)))
         (if err
             (progn
               (message (replique-edn/pr-str err))
               (message "completion failed with prefix %s" prefix))
           (funcall company-callback (gethash :candidates resp)))))
     t)))

(defun replique/auto-complete-clj (prefix company-callback props clj-repl)
  (replique/auto-complete* prefix company-callback props :clj-completion))

(defun replique/auto-complete-cljs (prefix company-callback props cljs-repl)
  (replique/auto-complete* prefix company-callback props :cljs-completion))

(defun replique/auto-complete (prefix company-callback)
  (replique/with-modes-dispatch
   (replique/mode . (-partial 'replique/auto-complete-session
                              prefix company-callback))
   (clojure-mode . (-partial 'replique/auto-complete-clj
                             prefix company-callback))
   (clojurescript-mode . (-partial 'replique/auto-complete-cljs
                                   prefix company-callback))))

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

(defun replique/eval-form (repl-type form)
  (message "Evaluating form %s ..." form)
  (-let* ((form (replace-regexp-in-string "\n" " " form))
          (repl (replique/get-active-repl-by-type repl-type t))
          (eval-chan (cdr (assoc :eval-chan repl)))
          (buff (cdr (assoc :buffer repl)))
          (res nil)
          (p (start-process "" nil nil)))
    (with-current-buffer buff
      (replique/comint-send-input-from-source form))
    (replique-async/<!
     eval-chan
     (lambda (msg)
       (setq res msg)
       (process-send-string p "\n")
       (delete-process p)))
    (accept-process-output p)
    (if (gethash :error res)
        (gethash :value res)
      (gethash :result res))))

(defvar replique/mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" 'replique/comint-send-input)
    map))

(define-derived-mode replique/mode comint-mode "Replique"
  "Commands:\\<replique/mode-map>"
  (setq comint-prompt-regexp replique/prompt)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-to-list 'company-backends 'replique/company-backend)
  )

(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" 'replique/eval-region)
    (define-key map "\C-x\C-e" 'replique/eval-last-sexp)
    (define-key map "\C-\M-x" 'replique/eval-defn)
    (define-key map "\C-c\C-l" 'replique/load-file)
    (define-key map "\C-c\M-n" 'replique/in-ns)
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
  (add-to-list 'company-backends 'replique/company-backend))

;;;###autoload
(define-minor-mode replique/generic-minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/generic-minor-mode-map}"
  :lighter "Replique" :keymap replique/generic-minor-mode-map)

(defun replique/is-valid-port-nb? (port-nb)
  (< -1 port-nb 65535))

(defun replique/guess-port-nb ()
  (let ((port-file (locate-dominating-file
                    default-directory ".replique-port")))
    (when port-file
      (with-temp-buffer
        (insert-file-contents (concat port-file ".replique-port"))
        (->> (buffer-string)
             replique-edn/read-string
             (gethash :repl))))))

(defvar replique/edn-tag-readers
  `((error . identity)
    (object . identity)))

(defun replique/dispatch-eval-msg* (in-chan out-chan)
  (replique-async/<!
   in-chan
   (lambda (msg)
     (cond ((equal :eval (gethash :type msg))
            (let ((repl (replique/get-repl-by-session
                         (gethash :repl-type msg)
                         (gethash :client (gethash :session msg)))))
              (when repl
                (setcdr (assoc :ns repl) (gethash :ns msg))
                (replique-async/put! (cdr (assoc :eval-chan repl)) msg)))
            (replique/dispatch-eval-msg* in-chan out-chan))
           (t (replique-async/put! out-chan msg)
              (replique/dispatch-eval-msg* in-chan out-chan))))))

(defun replique/dispatch-eval-msg (in-chan)
  (let ((out-chan (replique-async/chan)))
    (replique/dispatch-eval-msg* in-chan out-chan)
    out-chan))

(defun replique/edn-read-stream* (chan-in chan-out edn-state)
  (replique-async/<!
   chan-in
   (lambda (s)
     (let ((continue t))
       (while continue
         (let* ((reader (replique-edn/reader nil :str s))
                (result-state (assoc :result-state
                                     (symbol-value edn-state)))
                (result-state (if result-state (cdr result-state) nil)))
           (if (equal :waiting (symbol-value result-state))
               (replique-edn/set-reader edn-state reader)
             (-> (replique-edn/init-state reader edn-state)
                 (replique-edn/set-tagged-readers
                  replique/edn-tag-readers)))
           (replique-edn/read edn-state)
           (-let (((&alist :result result
                           :result-state result-state)
                   (symbol-value edn-state))
                  (rest-string
                   (replique-edn/reader-rest-string reader)))
             (when (and
                    (not (equal :waiting (symbol-value result-state)))
                    (car (symbol-value result)))
               (replique-async/put!
                chan-out (car (symbol-value result))))
             (if (not (string= "" rest-string))
                 (setq s rest-string)
               (setq continue nil)))))
       (replique/edn-read-stream* chan-in chan-out edn-state)))))

(defun replique/edn-read-stream (chan-in)
  (let ((edn-state (-> (replique-edn/reader nil :str "")
                       replique-edn/init-state
                       (replique-edn/set-tagged-readers
                        replique/edn-tag-readers)))
        (chan-out (replique-async/chan)))
    (replique/edn-read-stream* chan-in chan-out edn-state)
    chan-out))

(comment
 (defvar in-chan (replique-async/chan))
 (defvar out-chan (replique/edn-read-stream in-chan))

 (replique-async/<!
  out-chan
  (lambda (x)
    (print x)))

 (replique-async/put! in-chan "3 ")
 )

(defun replique/send-tooling-msg (props msg)
  (-let (((&alist :tooling-proc proc) props)
         (msg (replique/alist-to-map msg)))
    (process-send-string
     proc (format "(ewen.replique.server/tooling-msg-handle %s)\n"
                  (replique-edn/pr-str msg)))))

(defun replique/clj-buff-name (directory host port)
  (generate-new-buffer-name
   (format "%s|%s:%s|clj" (file-name-nondirectory directory) host port)))

(defun replique/cljs-buff-name (directory host port)
  (generate-new-buffer-name
   (format "%s|%s:%s|cljs" (file-name-nondirectory directory) host port)))

(comment
 (replique/cljs-buff-name "/home/egr" "127.0.0.1" 9000)
 )

(defun replique/cleanup-process (props)
  (let ((active-removed? nil))
    (-let (((&alist :active active
                    :tooling-proc tooling-proc
                    :clj-repls clj-repls
                    :cljs-repls cljs-repls)
            props))
      (when (and (equal clj-repls '())
                 (equal cljs-repls '()))
        (when (process-live-p tooling-proc)
          (set-process-filter
           tooling-proc (lambda (proc string) nil))
          (process-send-eof tooling-proc))
        (setq replique/processes
              (delete props replique/processes))
        (when active
          (let ((props (car replique/processes)))
            (when props
              (setcdr (assoc :active props) t))))))))

(defun replique/cleanup-repl (host port b)
  (-let* ((props (replique/process-props-by-host-port host port))
          ((&alist :clj-repls clj-repls
                   :cljs-repls cljs-repls
                   :active-clj-repl active-clj-repl
                   :active-cljs-repl active-cljs-repl)
           props)
          (repl-clj (-first (lambda (repl)
                          (eq b (cdr (assoc :buffer repl))))
                            clj-repls))
          (repl-cljs (-first (lambda (repl)
                          (eq b (cdr (assoc :buffer repl))))
                        cljs-repls)))
    (when repl-clj
      (let* ((buff (cdr (assoc :buffer repl-clj)))
             (proc (get-buffer-process buff)))
        (setcdr (assoc :clj-repls props)
                (delete repl-clj clj-repls))
        (when (and proc (process-live-p proc))
          (process-send-eof proc)))
      (when (eq active-clj-repl repl-clj)
        (setcdr (assoc :active-clj-repl props)
                (cadr (assoc :clj-repls props)))))
    (when repl-cljs
      (let* ((buff (cdr (assoc :buffer repl-cljs)))
             (proc (get-buffer-process buff)))
        (setcdr (assoc :cljs-repls props)
                (delete repl-cljs cljs-repls))
        (when (and proc (process-live-p proc))
          (process-send-eof proc)))
      (when (eq active-cljs-repl repl-cljs)
        (setcdr (assoc :active-cljs-repl props)
                (cadr (assoc :cljs-repls props)))))
    (replique/cleanup-process props)))

(defun replique/cleanup-repls (host port)
  (let ((props (replique/process-props-by-host-port host port)))
    (-let (((&alist :clj-repls clj-repls
                    :cljs-repls cljs-repls)
            props))
      (mapcar (lambda (repl)
                (replique/cleanup-repl
                 host port (cdr (assoc :buffer repl))))
              clj-repls)
      (mapcar (lambda (repl)
                (replique/cleanup-repl
                 host port (cdr (assoc :buffer repl))))
              cljs-repls))
    (replique/cleanup-process props)))

(defun replique/on-repl-close (host port buffer process event)
  (cond ((string= "deleted\n" event)
         (replique/cleanup-repl host port buffer))
        ((string= "connection broken by remote peer\n" event)
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-max))
             (insert (concat "\n" event "\n"))))
         (replique/cleanup-repl host port buffer))
        (t nil)))

(defun replique/make-repl
    (buffer-name host port repl-type init-ns &optional pop-buff)
  (-let* ((buff (make-comint (concat " " buffer-name) `(,host . ,port)))
          (proc (get-buffer-process buff))
          (chan-src (replique/process-filter-chan proc))
          (props (replique/process-props-by-host-port host port))
          ((&alist :clj-repls clj-repls :cljs-repls cljs-repls
                   :active-clj-repl active-clj-repl
                   :active-cljs-repl active-cljs-repl) props)
          (repl-cmd (format "(ewen.replique.server/repl %s)\n" repl-type)))
    (set-process-sentinel
     proc (-partial 'replique/on-repl-close host port buff))
    ;; Discard the prompt
    (replique-async/<!
     chan-src
     (lambda (x)
       (let ((chan (replique/edn-read-stream chan-src)))
         ;; Get the session number
         (process-send-string proc "(ewen.replique.server/tooling-repl)\n")
         (process-send-string proc "clojure.core.server/*session*\n")
         (replique-async/<!
          chan
          (lambda (resp)
            (let ((session (gethash :client resp)))
              ;; Reset process filter to the default one
              (set-process-filter proc 'comint-output-filter)
               (set-buffer buff)
               (replique/mode)
               (process-send-string proc repl-cmd)
               (let ((repl `((:type . ,repl-type)
                             (:session . ,session)
                             (:ns . ,init-ns)
                             (:buffer . ,buff)
                             (:eval-chan . ,(replique-async/chan)))))
                 (when (equal :clj repl-type)
                   (setcdr (assoc :clj-repls props)
                           (push repl clj-repls))
                   (setcdr (assoc :active-clj-repl props) repl))
                 (when (equal :cljs repl-type)
                   (setcdr (assoc :cljs-repls props)
                           (push repl clj-repls))
                   (setcdr (assoc :active-cljs-repl props) repl)))
               (when pop-buff
                 (pop-to-buffer buff))))))))))

(defun replique/repl-existing (props)
  (-let (((&alist :active active
                  :host host :port port
                  :clj-repls clj-repls :cljs-repls cljs-repls
                  :active-clj-repl active-clj-repl
                  :active-cljs-repl active-cljs-repl
                  :tooling-proc tooling-proc
                  :tooling-chan tooling-chan)
          props))
    (when (not active)
      (replique/set-active-process host port t))
    (replique/send-tooling-msg props `((:type . :repl-infos)))
    (replique-async/<!
     tooling-chan
     (lambda (resp)
       (let* ((directory (gethash :directory resp))
              (clj-buff-name (replique/clj-buff-name
                              directory host port))
              (cljs-buff-name (replique/cljs-buff-name
                               directory host port)))
         (when (equal '() clj-repls)
           (replique/make-repl clj-buff-name host
                               port :clj 'user t))
         (when (equal '() cljs-repls)
           (replique/cljs-buff-name directory host port)
           (replique/make-repl cljs-buff-name host
                               port :cljs 'cljs.user)))))))

(defun replique/on-tooling-repl-close
    (tooling-chan-src host port process event)
  (cond ((string= "deleted\n" event)
         (replique/cleanup-repls host port))
        ((string= "connection broken by remote peer\n" event)
         (replique/cleanup-repls host port))
        (t nil)))

(defun replique/make-tooling-proc (host port callback)
  (let* ((proc (open-network-stream "replique" nil host port))
         (chan (replique/process-filter-chan proc)))
    ;; Discard the prompt
    (replique-async/<!
     chan (lambda (x)
            (set-process-filter proc nil)
            (process-send-string
             proc
             "(ewen.replique.server/shared-tooling-repl)\n")
            (funcall callback proc)))))

;;;###autoload
(defun replique/repl (&optional host port)
  "Start a Clojure/Clojurescript REPL"
  (interactive
   (let ((host (read-string "Host: " "127.0.0.1"))
         (port (read-number "Port number: " (replique/guess-port-nb))))
     (list host port)))
  (if (not (replique/is-valid-port-nb? port))
      (message "Invalid port number: %d" port)
    (let ((existing-proc (replique/process-props-by-host-port host port)))
      (if existing-proc
          (replique/repl-existing existing-proc)
        (replique/make-tooling-proc
         host port
         (lambda (tooling-proc)
           (let* ((tooling-chan-src (replique/process-filter-chan
                                     tooling-proc))
                  (tooling-chan (-> tooling-chan-src
                                    replique/edn-read-stream
                                    replique/dispatch-eval-msg)))
             (set-process-sentinel
              tooling-proc
              (-partial 'replique/on-tooling-repl-close
                        tooling-chan-src host port))
             (replique/send-tooling-msg
              `((:tooling-proc . ,tooling-proc))
              `((:type . :repl-infos)))
             (replique-async/<!
              tooling-chan
              (lambda (resp)
                (let* ((directory (gethash :directory resp))
                       (repl-type (gethash :repl-type resp))
                       (clj-buff-name
                        (replique/clj-buff-name directory host port))
                       (cljs-buff-name
                        (replique/cljs-buff-name directory host port))
                       (buff-props `((:directory . ,directory)
                                     (:active . ,t)
                                     (:tooling-proc . ,tooling-proc)
                                     (:host . ,host)
                                     (:port . ,port)
                                     (:tooling-chan . ,tooling-chan)
                                     (:clj-repls . ,'())
                                     (:cljs-repls . ,'())
                                     (:active-clj-repl . ,nil)
                                     (:active-cljs-repl . ,nil))))
                  (push buff-props replique/processes)
                  (replique/set-active-process host port nil)
                  (cond ((equal :clj repl-type)
                         (replique/make-repl
                          clj-buff-name host port :clj 'user t))
                        ((equal :cljs repl-type)
                         (replique/make-repl
                          clj-buff-name host port :clj 'user)
                         (replique/make-repl
                          cljs-buff-name host port :cljs 'cljs.user t))
                        (t (replique/cleanup-repls host port)
                           (error "Error while starting the REPL. Invalid REPL type: %s"
                                  repl-type)))))))))))))

(comment (provide 'replique))

;;; replique.el ends here
