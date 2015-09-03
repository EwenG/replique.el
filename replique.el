;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.11.0") (company "0.8.12") (dash-functional "1.2.0") (s "1.9.0"))
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'replique-runnables)
(require 'replique-edn)
(require 'replique-helm)
(require 'replique-async)
(require 'replique-comint)






(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)


(defmacro replique/when-lambda (match-form &rest body)
  "Like -lambda, but call the body only if no parameter is nil.
Otherwise, the lambda simply returns nil."
  (let ((tmp-args (make-symbol "args")))
    `(lambda (&rest ,tmp-args)
       (if (-none? 'null ,tmp-args)
           (funcall (-lambda (,match-form)
                      ,@body)
                    ,tmp-args)
         nil))))

(defmacro replique/when-let (var-val &rest body)
  "Same as -when-let but performs the nil check BEFORE the destructuring binding, ie: the body is executed even if vars are bound to nil after the destructuring occured."
  (let ((var (car var-val))
        (val (cadr var-val))
        (tmp-val (make-symbol "val")))
    `(let ((,tmp-val ,val))
       (when ,tmp-val
         (-let ((,var ,tmp-val))
           ,@body)))))

(defun replique/assoc (alist key val)
  (assq-delete-all key alist)
  (cons `(,key . ,val) alist))

(defun replique/replique-root-dir ()
  (-> (locate-library "replique")
      file-name-directory))

(defun replique/last (seq)
  (cond ((equal 0 (length seq)) nil)
        (t (elt seq (- (length seq) 1)))))

(defun replique/alist-to-map (alist)
  (let ((m (make-hash-table :test 'equal)))
    (mapcar (-lambda ((k . v))
              (puthash k v m))
            alist)
    m))

;;; Reads a string from the user.
(defun replique/symprompt (prompt default)
  (list (let* ((prompt (if default
                           (format "%s (default %s): " prompt default)
                         (concat prompt ": ")))
               (ans (read-string prompt)))
          (if (zerop (length ans)) default ans))))

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

(defun replique/error-message (error-map)
  (->> (gethash :via error-map)
       (funcall (-rpartial 'elt 0))
       (gethash :message)))








(defgroup replique nil
  ""
  :group 'clojure)













(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

(defcustom replique/cljs-comp-opts
  '((:output-dir . "out")
    (:output-to . "out/main.js")
    (:recompile-dependents . nil)
    (:main . (ewen.replique.cljs-env.browser))
    (:asset-path . "."))
  "Clojurescript compiler options."
  :type '(alist :output-dir string
                :output-to string
                :recompile-dependents boolean
                :main (repeat symbol)
                :asset-path string)
  :group 'replique)

(defcustom replique/cljs-browser-repl-opts
  '((:port . 9000)
    (:static-dir . ("out"))
    (:src . nil))
  "Clojurescript browser REPL options."
  :type '(alist :port integer
                :static-dir (repeat string)
                :src (choice (const :tag "None" nil)
                             string))
  :group 'replique)

(defun replique/get-cljs-comp-opts (cljs-env overrides)
  (cond ((string= "browser-env" cljs-env)
         replique/cljs-comp-opts)
        ((string= "webapp-env" cljs-env)
         (-let (((&alist ':output-dir output-dir
                         ':output-to output-to
                         ':main mains) overrides)
                ((&alist ':recompile-dependents
                         recompile-dependents
                         ':asset-path asset-path)
                 replique/cljs-comp-opts))
           `((:output-dir . ,output-dir)
             (:output-to . ,output-to)
             (:recompile-dependents . ,recompile-dependents)
             (:main . ,mains)
             (:asset-path . ,asset-path))))
        (t (error "Unsupported environement: %s" cljs-env))))

(defun replique/to-js-file-name (file-name)
  (if (s-ends-with? ".js" file-name)
      file-name
    (concat file-name ".js")))















(defun replique/comint-is-closed-sexpr (start limit)
  (let ((depth (car (parse-partial-sexp start limit))))
    (if (<= depth 0) t nil)))

(defun replique/comint-send-input (&optional no-newline artificial)
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (let* ((pmark (process-mark proc)))
        (cond (;; Point is at the end of the line and the sexpr is
               ;; terminated
               (and (equal (point) (point-max))
                    (replique/comint-is-closed-sexpr pmark (point)))
               (comint-send-input no-newline artificial))
              ;; Point is after the prompt but (before the end of line or
              ;; the sexpr is not terminated)
              ((comint-after-pmark-p) (comint-accumulate))
              ;Point is before the prompt. Do nothing.
              (t nil))))))

(defun replique/comint-send-input-from-source
    (input &optional no-newline artificial)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (user-error "Current buffer has no process")
      (widen)
      (comint-add-to-input-history input)
      (run-hook-with-args 'comint-input-filter-functions
                          (if no-newline input
                            (concat input "\n")))

      (comint-snapshot-last-prompt)

      (setq comint-save-input-ring-index comint-input-ring-index)
      (setq comint-input-ring-index nil)

      (let ((comint-input-sender-no-newline no-newline))
        (funcall comint-input-sender proc input))

      ;; This used to call comint-output-filter-functions,
      ;; but that scrolled the buffer in undesirable ways.
      (run-hook-with-args 'comint-output-filter-functions ""))))

(defun replique/comint-refresh-prompt ()
  (let ((old-input (funcall comint-get-old-input))
        (process (get-buffer-process (current-buffer))))
    (if (not process)
        (user-error "Current buffer has no process")
      (comint-kill-input)
      (comint-send-input)
      (goto-char (process-mark process))
      (insert old-input))))

(defvar replique/edn-tag-readers
  `((ewen.replique.core.ToolingMsg
     . ,(-lambda (msg)
          `((type . ,(gethash :type msg))
            (uid . ,(gethash :uid msg))
            (result . ,(gethash :result msg))
            (platform . ,(gethash :platform msg))
            (error . ,(gethash :error msg)))))
    (error . ,(-lambda (msg)
                `((message . ,(->> (gethash :via msg)
                                   (funcall (-rpartial 'elt 0))
                                   (gethash :message))))))
    (object . identity)))

(comment
 (let* ((state (-> (replique-edn/reader nil :str "#ewen.replique.core.ToolingMsg{:type \"load-file\" :result \"#'test-project.core/foo\"}")
                   replique-edn/init-state
                   (replique-edn/set-tagged-readers
                    replique/edn-tag-readers)))
        (msg (-> (replique-edn/read state)
                 replique-edn/result)))
   (cdr (assoc 'type msg)))
 )


(defvar replique/mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique/mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" #'replique/comint-send-input)
    (define-key map "\C-x\C-e" #'replique/eval-last-sexp)
    map))


(define-derived-mode replique/mode comint-mode "Replique"
  "Commands:\\<replique/mode-map>"
  (setq comint-prompt-regexp replique/prompt)
  (setq comint-prompt-read-only t)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-to-list 'company-backends 'replique/company-backend))












(defun replique/platform-jar (platform)
  (caar (replique-runnables/jars-in-path platform)))

(defun replique/repl-cmd-raw (jar platform init-opts)
  (let ((platform-suffix (if (string= "cljs" platform) "-cljs" "")))
    `("java" "-cp"
      ,(format "%s:%slib/json-java.jar" jar (replique/replique-root-dir))
      "clojure.main" "-e"
      ,(format "(do (load-file \"%sclojure/ewen/replique/init%s.clj\") (ewen.replique.init/init %s %s))"
               (replique/replique-root-dir)
               platform-suffix
               (-> (replique/replique-root-dir)
                   replique-edn/pr-str)
               init-opts))))

(defun replique/repl-cmd-lein (lein platform init-opts)
  (let ((platform-suffix
         (if (string= "cljs" platform)
             "-cljs" "")))
    `(,lein "run" "-m" "clojure.main/main" "-e"
      ,(format "(do (load-file \"%sclojure/ewen/replique/init%s.clj\") (ewen.replique.init/init %s %s))"
               (replique/replique-root-dir)
               platform-suffix
               (-> (replique/replique-root-dir)
                   replique-edn/pr-str)
               init-opts))))

(defun replique/guess-project-root-dir ()
  (or (locate-dominating-file default-directory "project.clj")
      default-directory))

(defun replique/project-repl-cmd (root-dir platform init-opts)
  (let* ((init-opts (-> (replique/alist-to-map init-opts)
                        replique-edn/pr-str
                        replique-edn/pr-str)))
    (if (locate-file "project.clj" (list root-dir))
        (let ((lein-script (replique-runnables/lein-in-path)))
          (if lein-script
              (replique/repl-cmd-lein lein-script platform init-opts)
            'lein-script-not-found))
      (let ((runnable-jar (replique/platform-jar platform)))
        (if (null runnable-jar)
            'runnable-jar-not-found
          (replique/repl-cmd-raw runnable-jar platform init-opts))))))

(defun replique/repl* (root-dir platform repl-cmd)
  (when (not repl-cmd)
    (error "Clojure process cannot be started"))
  (let* ((default-directory root-dir)
         (buffer-name (replique/gen-buffer-name))
         (comint-buffer (apply #'make-comint
                               buffer-name
                               (car repl-cmd) nil (cdr repl-cmd)))
         (edn-reader-state (make-symbol "edn-reader-state"))
         (_ (set edn-reader-state nil))
         (buff-props
          `((name . ,buffer-name)
            (buffer . ,comint-buffer)
            (root-dir . ,root-dir)
            (platform . ,platform)
            (active . t)
            (sourcepaths . nil)
            (resourcepaths . nil)
            (tooling-chans . ,(make-hash-table :test 'equal))
            (edn-reader-state . ,edn-reader-state))))
    (set-buffer comint-buffer)
    (replique/mode)
    (set-process-filter
     (get-buffer-process comint-buffer)
     (-partial 'replique-comint/output-filter-dispatch buff-props))
    (push buff-props replique/buffers)
    (replique/set-active-buffer buffer-name)
    (pop-to-buffer comint-buffer)
    (when (not replique/buffers-hook-added)
      (add-hook
       'kill-buffer-hook
       (lambda ()
         (-let* ((b (current-buffer))
                 (buff-props (replique/buffer-props-by
                              (-lambda ((&alist 'buffer buffer))
                                (equal buffer b))))
                 ((&alist 'tooling-chans tooling-chans)
                  buff-props))
           (when tooling-chans
             (maphash (lambda (k v)
                        (replique-async/close! v))
                      tooling-chans))
           (setq replique/buffers
                 (delete buff-props
                         replique/buffers))
           (when (equal b (replique/get-active-buffer))
             (replique/set-active-buffer
              (->> (car replique/buffers)
                   (assoc 'name))))))))
    (setq replique/buffers-hook-added t)))

(defun replique/platform-to-name (platform)
  (cond ((equal platform "cljs")
         "Clojurescript")
        ((equal platform "clj")
         "Clojure")
        (t (error "Unknown platform: %s" platform))))

(defun replique/handle-jar-not-found (root-dir platform init-opts)
  (when (yes-or-no-p (format "Sorry, No %s jar could be found on the filesystem in order to start the REPL. Would you like to download a %s jar now?"
                             (replique/platform-to-name platform)
                             (replique/platform-to-name platform)))
    (-> (ido-read-directory-name
         (format "Please enter the directory where the %s jar will be saved: "
                 (replique/platform-to-name platform)))
        (replique-runnables/download-jar
         platform
         (lambda (jar-path)
           (if (equal platform "cljs")
               (replique/repl-cljs
                (replique/repl-cmd-raw jar-path platform
                                       (-> (replique/alist-to-map init-opts)
                                           replique-edn/pr-str
                                           replique-edn/pr-str))
                root-dir)
             (replique/repl
              (replique/repl-cmd-raw jar-path platform
                                     (-> (replique/alist-to-map init-opts)
                                         replique-edn/pr-str
                                         replique-edn/pr-str))
              root-dir))))))
  'runnable-jar-not-found)

(defun replique/handle-lein-not-found (root-dir platform init-opts)
  (when (yes-or-no-p (format "Sorry, leiningen could not be found on the filesystem in order to start the REPL. Would you like to install it now?"))
    (let* ((lein-copy-target (ido-read-directory-name
                              (format "Please enter the directory where the lein script will be saved: ")))
           (lein-copy-target (concat lein-copy-target "lein")))
      (copy-file
       (concat (replique/replique-root-dir) "runnables/lein")
       lein-copy-target
       1)
      (replique/repl
       (replique/repl-cmd-lein
        lein-copy-target platform
        (-> (replique/alist-to-map init-opts)
            replique-edn/pr-str
            replique-edn/pr-str))
       root-dir)))
  'lein-script-not-found)

;;;###autoload
(defun replique/repl (&optional repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (ido-read-directory-name
                       "REPL root directory: "
                       (replique/guess-project-root-dir)))
            (repl-cmd (replique/project-repl-cmd root-dir "clj" nil)))
       (list repl-cmd root-dir))))
  (cond ((equal 'runnable-jar-not-found repl-cmd)
         (replique/handle-jar-not-found
          root-dir "clj" nil))
        ((equal 'lein-script-not-found repl-cmd)
         (replique/handle-lein-not-found
          root-dir "clj" nil))
        (t (replique/repl* root-dir "clj" repl-cmd))))

;;;###autoload
(defun replique/repl-cljs (&optional repl-cmd root-dir)
  "Run a Clojurescript REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (ido-read-directory-name
                       "REPL root directory: "
                       (replique/guess-project-root-dir)))
            (cljs-env (ido-completing-read
                       "Clojurescript execution environment: "
                       '("browser-env" "webapp-env")
                       nil t))
            (output-dir (when (string= "webapp-env" cljs-env)
                          (ido-read-directory-name
                           "Clojurescript output-dir: "
                           root-dir)))
            (output-to (when (string= "webapp-env" cljs-env)
                         (-> (ido-read-file-name
                              "Clojurescript output-to file: "
                              output-dir
                              nil
                              nil
                              "main.js")
                             replique/to-js-file-name)))
            (mains (when (string= "webapp-env" cljs-env)
                     (replique-helm/cljs-select-ns root-dir)))
            (overrides (when (string= "webapp-env" cljs-env)
                         `((:output-dir . ,output-dir)
                           (:output-to . ,output-to)
                           (:main . ,mains))))
            (comp-opts (replique/get-cljs-comp-opts cljs-env overrides))
            (init-opts `((:cljs-env-name . ,cljs-env)
                         (:comp-opts . ,(replique/alist-to-map
                                         comp-opts))
                         (:repl-opts
                          . ,(replique/alist-to-map
                              replique/cljs-browser-repl-opts))))
            (repl-cmd (replique/project-repl-cmd root-dir "cljs" init-opts))
            (repl-cmd (cond ((equal 'runnable-jar-not-found repl-cmd)
                             (replique/handle-jar-not-found
                              root-dir "cljs" init-opts))
                            ((equal 'lein-script-not-found repl-cmd)
                             (replique/handle-lein-not-found
                              root-dir "cljs" init-opts))
                            (t repl-cmd))))
       (list repl-cmd root-dir))))
  (cond ((equal 'lein-script-not-found repl-cmd) nil)
        ((equal 'runnable-jar-not-found repl-cmd) nil)
        (t (replique/repl* root-dir "cljs" repl-cmd))))


(defvar replique/buffers-hook-added nil)
(defvar replique/buffers nil)

(defun replique/gen-buffer-name ()
  (let* ((buffer-prefix "replique")
         (buffer-names (mapcar (-lambda ((&alist 'name name))
                                 name)
                               replique/buffers))
         (new-buffer-name buffer-prefix)
         (buffer-nb 0))
    (while (member new-buffer-name buffer-names)
      (setq buffer-nb (1+ buffer-nb))
      (setq new-buffer-name (format "%s<%d>" buffer-prefix buffer-nb)))
    new-buffer-name))

(defun replique/set-active-buffer (buffer-name &optional display-msg)
  (interactive
   (let ((buffer-names (mapcar
                        (-lambda ((&alist 'name name))
                          name)
                        replique/buffers)))
     (list
      (ido-completing-read
       "Set active buffer to: "
       buffer-names nil t)
      t)))
  (setq replique/buffers
        (mapcar
         (-lambda (buff)
           (-let (((&alist 'name name) buff))
             (if (equal buffer-name name)
                 (replique/assoc buff 'active t)
               (replique/assoc buff 'active nil))))
         replique/buffers))
  (when display-msg
    (message "Active buffer switched to: %s" buffer-name)))

(defun replique/buffer-props-by (pred &optional error-on-nil)
  (let ((props (-first pred replique/buffers)))
    (if (and (null props) error-on-nil)
        (error "No Clojure subprocess")
      props)))

(defun replique/active-buffer-props (&optional error-on-nil)
  (replique/buffer-props-by
   (-lambda ((&alist 'active active))
     active)
   error-on-nil))

(defun replique/current-or-active-buffer-props
    (&optional error-on-nil)
  (if (derived-mode-p 'replique/mode)
      (replique/buffer-props-by
       (-lambda ((&alist 'buffer buffer))
         (equal (current-buffer) buffer))
       t)
    (replique/active-buffer-props t)))

(defun replique/get-active-buffer (&optional error-on-nil)
  (->> (replique/active-buffer-props error-on-nil)
       (assoc 'buffer)
       cdr))

(defun replique/get-project-root-dir (&optional error-on-nil)
  (->> (replique/active-buffer-props error-on-nil)
       (assoc 'root-dir)
       cdr))

(defun replique/get-project-type (&optional error-on-nil)
  (let ((root-dir (replique/get-project-root-dir error-on-nil)))
    (if (locate-file "project.clj" (list root-dir))
        "leiningen"
      "raw")))

(defun replique/get-project-platform (&optional error-on-nil)
  (->> (replique/active-buffer-props error-on-nil)
       (assoc 'platform)
       cdr))

(defun replique/get-in-project (prop &optional error-on-nil)
  (->> (replique/active-buffer-props error-on-nil)
       (assoc prop)
       cdr))




















(defun replique/eval-region (start end)
  "Eval the currently highlighted region."
  (interactive "r")
  (let ((input (filter-buffer-substring start end)))
    (with-current-buffer
        (replique/get-active-buffer)
      (replique/comint-send-input-from-source input))))

(defun replique/eval-last-sexp ()
  "Eval the previous sexp."
  (interactive)
  (replique/eval-region
   (save-excursion
     (backward-sexp) (point))
   (point)))

(defun replique/eval-defn ()
  "Eval the current defn expression."
  (interactive)
  (let ((input (->> (thing-at-point 'defun)
                    (replace-regexp-in-string "\n" ""))))
    (with-current-buffer
        (replique/get-active-buffer)
      (replique/comint-send-input-from-source input))))

(defvar replique/prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `replique/load-file' command.")

(defun replique/init-load-file (file-type file-name)
  ;; Check to see if buffer needs saved.
  (comint-check-source file-name)
  (setq replique/prev-l/c-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  (message "Loading %s file: %s ..." file-type file-name))

(defun replique/load-file (file-name)
  (interactive (comint-get-source
                "Load file: "
                replique/prev-l/c-dir/file
                (list major-mode) t))
  "Load a Clojure file into the Clojure process."
  (replique/init-load-file "Clojure" file-name)
  (-let* ((chan (replique-async/chan))
          (buff-props (replique/current-or-active-buffer-props t))
          ((&alist 'buffer buffer) buff-props)
          (proc (get-buffer-process buffer)))
    (replique-comint/tooling-send-msg
     buff-props
     `((:type . "load-file")
       (:file-path . ,file-name))
     chan)
    (replique-async/<!
     chan
     (replique/when-lambda
      ((&alist 'error err'result result))
      (if err
          (progn (comint-output-filter
                  proc
                  (-> err
                      replique/error-message
                      (concat "\n")))
                 (message
                  "Loading Clojure file: %s ... Failed." file-name))
        (progn
          (comint-output-filter
           proc (-> result
                    replique-edn/pr-str
                    (concat "\n")))
          (message "Loading Clojure file: %s ... Done." file-name)))))))

(defun replique/list-css ()
  (if (not (string= "cljs" (replique/get-in-project 'platform t)))
      (error "Not a Clojurescript process")
    (let ((chan (replique-async/chan)))
      (replique-comint/tooling-send-msg
       (replique/current-or-active-buffer-props t)
       `((:type . "list-css"))
       chan)
      (replique/when-let
       ((&alist 'error err
                'result result)
        (replique-async/<!! chan))
       (if err
           (error "List css failed")
         result)))))

(defun replique/load-css (file-name)
  (let* ((file-name (concat "file:" file-name))
         (css-list (replique/list-css))
         (css-list (if (-contains? css-list file-name)
                       css-list
                     (cons "*new-css*" css-list)))
         (candidates (-sort (-partial
                             'replique/uri-sort-fn
                             file-name)
                            css-list))
         (css-file (ido-completing-read
                    "Reload css file: "
                    candidates
                    nil t))
         (css-file (if (equal "*new-css*" css-file)
                       file-name
                     css-file)))
    (message "Loading css file: %s ..." file-name)
    (let ((chan (replique-async/chan)))
      (replique-comint/tooling-send-msg
       (replique/current-or-active-buffer-props t)
       `((:type . "load-file-generic")
         (:file-type . "css")
         (:file-path . ,file-name)
         (:css-file . ,css-file))
       chan)
      (replique-async/<!
       chan
       (replique/when-lambda
        ((&alist 'error err))
        (if err
            (message "Loading css file: %s ... Failed." file-name)
          (message "Loading css file: %s ... Done." file-name)))))))

(defun replique/load-js (file-name)
  (replique/init-load-file "Javascript" file-name)
  (let ((chan (replique-async/chan)))
    (replique-comint/tooling-send-msg
     (replique/current-or-active-buffer-props t)
     `((:type . "load-file-generic")
       (:file-type . "js")
       (:file-path . ,file-name))
     chan)
    (replique-async/<!
     chan
     (replique/when-lambda
      ((&alist 'error err))
      (if err
          (message "Loading js file: %s ... Failed." file-name)
        (message "Loading js file: %s ... Done." file-name))))))

(defun replique/load-file-generic (file-name)
  (interactive (if (string-suffix-p ".css" (buffer-file-name) t)
                   (list (buffer-file-name))
                 (comint-get-source
                  "Load file: "
                  replique/prev-l/c-dir/file
                  (list major-mode) t)))
  (cond ((string-suffix-p ".js" file-name t)
         (if (not (string= "cljs" (replique/get-in-project 'platform)))
             (error "Not a Clojurescript process")
           (replique/load-js file-name)))
        ((string-suffix-p ".css" file-name t)
         (if (not (string= "cljs" (replique/get-in-project 'platform)))
             (error "Not a Clojurescript process")
           (replique/load-css file-name)))
        (t (message "Cannot recognize the type of the file: %s"
                    file-name))))

(defun replique/set-ns (ns)
  "Set the ns of the Clojure process.
Defaults to the ns of the current buffer."
  (interactive (replique/symprompt "Set ns to" (clojure-find-ns)))
  (message "Setting namespace to: %s ..." ns)
  (-let* ((chan (replique-async/chan))
          (buff-props (replique/current-or-active-buffer-props t))
          ((&alist 'buffer buffer) buff-props)
          (proc (get-buffer-process buffer)))
    (replique-comint/tooling-send-msg
     buff-props
     `((:type . "set-ns")
       (:ns . ,ns))
     chan)
    (replique-async/<!
     chan
     (replique/when-lambda
      ((&alist 'error err))
      (if err
          (message "Setting namespace to: %s ... Failed." ns)
        (progn
          (comint-output-filter proc "\n")
          (message "Setting namespace to: %s ... Done." ns)))))))

(defun replique/add-sourcepath (path)
  (interactive
   (let ((project-type (replique/get-project-type t)))
     (if (string= project-type "raw")
         (list (ido-read-file-name "Add sourcepath: "))
       (list 'error))))
  (if (equal 'error path)
      (find-file-other-window
       (->
        (replique/get-project-root-dir t)
        (concat "project.clj")))
    (let ((buff (replique/current-or-active-buffer-props t)))
      (message "Adding sourcepath: %s ..." path)
      (let ((chan (replique-async/chan)))
        (replique-comint/tooling-send-msg
         buff
         `((:type . "add-classpath")
           (:path . ,path))
         chan)
        (replique-async/<!
         chan
         (replique/when-lambda
          ((&alist 'result result 'error err))
          (if (and (not (null result))
                   (not err))
              (let ((sourcepaths (assoc 'sourcepaths buff)))
                (setcdr sourcepaths
                        (-union (cdr sourcepaths) (list path)))
                (message "Adding sourcepath: %s ... Done." path))
            (message "Adding sourcepath: %s ... Failed." path))))))))

(defun replique/add-resourcepath (path)
  (interactive
   (let ((project-type (replique/get-project-type t)))
     (if (string= project-type "raw")
         (list (ido-read-file-name "Add resourcepath: "))
       (list 'error))))
  (if (equal 'error path)
      (find-file-other-window
       (->
        (replique/get-project-root-dir t)
        (concat "project.clj")))
    (let ((buff (replique/active-buffer-props t)))
      (message "Adding resourcepath: %s ..." path)
      (let ((chan (replique-async/chan)))
        (replique-comint/tooling-send-msg
         buff
         `((:type . "add-classpath")
           (:path . ,path))
         chan)
        (replique-async/<!
         chan
         (replique/when-lambda
          ((&alist 'result result 'error err))
          (if (and (not (null result))
                   (not err))
              (let ((resourcepaths (assoc 'resourcepaths buff)))
                (setcdr
                 resourcepaths
                 (-union (cdr resourcepaths)
                         (list path)))
                (message
                 "Adding resourcepath: %s ... Done." path))
            (message "Adding resourcepath: %s ... Failed." path))))))))

(defun replique/reload-project ()
  (interactive)
  (if (string= "raw" (replique/get-project-type t))
      (message "Reloading project ... Nothing to do")
    (let ((file-path (-> (replique/get-project-root-dir t)
                         (concat "project.clj"))))
      (message "Reloading project ...")
      (let ((chan (replique-async/chan)))
        (replique-comint/tooling-send-msg
         (replique/active-buffer-props t)
         `((:type . "reload-project")
           (:file-path . ,file-path))
         chan)
        (replique-async/<!
         chan
         (replique/when-lambda
          ((&alist 'result result 'error err))
          (if (and (not (null result))
                   (not err))
              (message "Reloading project %s ... Done." file-path)
            (message "Reloading project %s ... Failed." file-path))))))))

(defun replique/eval-form (form)
  (message "Evaluating form %s ..." form)
  (-let* ((form (replace-regexp-in-string "\n" " " form))
          (chan (replique-async/chan))
          (buff-props (replique/current-or-active-buffer-props t))
          ((&alist 'buffer buffer) buff-props)
          (proc (get-buffer-process buffer)))
    (replique-comint/tooling-send-msg
     buff-props
     `((:type . "eval-form")
       (:form . ,form))
     chan)
    (replique/when-let
     ((&alist 'error err 'result result)
      (replique-async/<!! chan))
     (message "Evaluating form %s ... Done." form)
     (if err
         (replique/error-message err)
       result))))





(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" #'replique/eval-region)
    (define-key map "\C-x\C-e" #'replique/eval-last-sexp)
    (define-key map "\C-\M-x" #'replique/eval-defn)
    (define-key map "\C-c\C-l" #'replique/load-file)
    (define-key map "\C-c\M-n" #'replique/set-ns)
    (easy-menu-define replique/minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique/eval-region t]
        ["Eval last sexp" replique/eval-last-sexp t]
        ["Eval defn" replique/eval-defn t]
        "--"
        ["Load file" replique/load-file t]
        "--"
        ["Set REPL ns" replique/set-ns t]))
    map))

(defvar replique/generic-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-l" #'replique/load-file)
    (easy-menu-define replique/generic-minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Load file" replique/load-file-generic t]))
    map))



;;;###autoload
(define-minor-mode replique/minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/minor-mode-map}"
  :lighter "Replique" :keymap replique/minor-mode-map
  (make-local-variable 'company-backends))

;;;###autoload
(define-minor-mode replique/generic-minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/generic-minor-mode-map}"
  :lighter "Replique" :keymap replique/generic-minor-mode-map)













;; Auto completion
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
  (skip-chars-backward (concat "^" " \t\n\";{}()[]^\@/`~"))
  (replique/skip-regexp-forward "#_\\|#\\|'"))

(defun replique/symbol-backward ()
  (save-excursion
    (let ((end (point)))
      (replique/skip-symbol-backward)
      (when (not (equal end (point)))
        (buffer-substring-no-properties (point) end)))))

(defun replique/send-completions (prefix company-callback)
  (let ((chan (replique-async/chan)))
    (replique-comint/tooling-send-msg
     (replique/current-or-active-buffer-props t)
     `((:type . "completions")
       (:prefix . ,prefix))
     chan)
    (replique-async/<!
     chan
     (replique/when-lambda
      ((&alist 'error err
               'result result))
      (when (not err)
        (funcall company-callback result))))))

(defun replique/company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'replique/company-backend))
    (prefix (when (or (derived-mode-p 'clojure-mode)
                      (derived-mode-p 'replique/mode))
              (replique/symbol-backward)))
    (candidates `(:async . ,(-partial
                             'replique/send-completions
                             (replique/symbol-backward))))))










(defun replique/read-defproject ()
  (when (string= (replique/get-project-type) "leiningen")
      (let* ((f-path (-> (replique/get-project-root-dir)
                         (concat "project.clj")))
             (defproject (with-temp-buffer
                           (insert-file-contents f-path)
                           (search-forward "defproject " nil t)
                           (when (not (bobp))
                             (backward-char)
                             (->> (thing-at-point 'defun)
                                  (replique-edn/reader nil :str)
                                  replique-edn/init-state
                                  replique-edn/read
                                  replique-edn/result))))
             (args (cdddr defproject))
             (keys (mapcar 'car (-partition 2 args)))
             (unique-keys (-distinct keys)))
        (if (equal (length unique-keys) (length keys))
            (->> (-partition 2 args)
                 (mapcar (-lambda ((k v))
                           (cons k v))))
          (error "Found duplicate keys in project.clj")))))


(defun replique/format-dependencies (dependencies)
  (let ((suffix "\n                 ")
        deps-str)
    (dolist (dep dependencies deps-str)
      (setq deps-str (concat
                      deps-str
                      (replique-edn/pr-str dep)))
      (setq deps-str (concat
                      deps-str
                      suffix)))
    (or (s-chop-suffix suffix deps-str)
        "")))

(defun replique/format-src-res-paths (paths)
  (let ((suffix "\n                   ")
        paths-str)
    (dolist (path paths paths-str)
      (setq paths-str (concat paths-str (format "\"%s\"" path)))
      (setq paths-str (concat paths-str suffix)))
    (or (s-chop-suffix suffix paths-str)
        "")))

(defun replique/format-project-file (dependencies src-paths res-paths)
  (format
   "(defproject org.example/sample \"0.0.1-SNAPSHOT\"
  :dependencies [%s]
  :source-paths   [%s]
  :resource-paths [%s])"
   (replique/format-dependencies dependencies)
   (replique/format-src-res-paths src-paths)
   (replique/format-src-res-paths res-paths)))

(defun replique/project-file ()
  (interactive)
  (let ((project-type (replique/get-project-type t))
        (platform (replique/get-project-platform t)))
    (if (string= project-type "raw")
        (let ((project-def (replique/format-project-file
                            (-> (replique-runnables/default-dep platform)
                                list)
                            (replique/get-in-project 'sourcepaths t)
                            (replique/get-in-project 'resourcepaths t))))
          (find-file-other-window
           (format "%sproject.clj"
                   (replique/get-project-root-dir t)))
          (erase-buffer)
          (insert project-def))
      (find-file-other-window
       (->
        (replique/get-project-root-dir t)
        (concat "project.clj"))))))


(provide 'replique)

;;; replique.el ends here
