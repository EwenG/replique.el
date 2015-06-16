;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.10.0") (company "0.8.12") (dash-functional "1.2.0") (s "1.9.0") (edn "1.1))
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'replique-runnables)
(require 'replique-edn)






(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

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









(defgroup replique nil
  ""
  :group 'clojure)













(defcustom replique/prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)















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
  `((:readers . ((ewen.replique.core.ToolingMsg . ,(-lambda (msg)
                                                     `((type . ,(gethash :type msg))
                                                       (result . ,(gethash :result msg))
                                                       (platform . ,(gethash :platform msg))
                                                       (error . ,(gethash :error msg)))))
                 (error . ,(-lambda (msg)
                             `((message . ,(->> (gethash :via msg)
                                                (funcall (-rpartial 'elt 0))
                                                (gethash :message))))))
                 (object . identity)))))

(comment
 (let* ((rdr (replique-edn/reader nil :str "#ewen.replique.core.ToolingMsg{:type \"load-file\" :result \"#'test-project.core/foo\"}"))
        (msg (replique-edn/read rdr replique/edn-tag-readers)))
                                        ;(cdr (assoc 'type msg))
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













(defun replique/repl-cmd-raw (jar platform)
  (if (null jar)
      nil
      (let ((platform-suffix (if (string= "cljs" platform) "-cljs" "")))
        `("java" "-cp" ,jar "clojure.main" "-e"
          ,(format "(do (load-file \"%sclojure/ewen/replique/init%s.clj\") (ewen.replique.init/init \"%s\"))" (replique/replique-root-dir) platform-suffix (replique/replique-root-dir))))))

(defvar replique/clojure-build-tools
  (cl-flet ((platform-jar (platform)
                          (caar
                           (replique-runnables/jars-in-path platform))))
    `(((name . "leiningen")
       (project-file . "project.clj")
       (default-repl-cmd
         . ,(lambda (platform)
              (let ((platform-suffix
                     (if (string= "cljs" platform)
                         "-cljs" "")))
                `("lein" "run" "-m" "clojure.main/main" "-e"
                  ,(format "(do (load-file \"%sclojure/ewen/replique/init%s.clj\") (ewen.replique.init/init \"%s\"))" (replique/replique-root-dir) platform-suffix (replique/replique-root-dir)))))))

      ((name . "raw")
       (project-file . nil)
       (default-repl-cmd
         . ,(lambda (platform)
              (replique/repl-cmd-raw
               (platform-jar platform)
               platform)))))))

(defun replique/build-files ()
  (->> (mapcar (-lambda ((&alist 'project-file project-file))
                 project-file)
               replique/clojure-build-tools)
       (delete nil)))



















(defun replique/guess-project-root-dir ()
  (->> (mapcar (lambda (file)
                 (locate-dominating-file default-directory file))
               (replique/build-files))
       (remove nil)
       car
       (or default-directory)))

(defun replique/project-repl-cmd (root-dir platform)
  (let* ((repl-cmds (mapcar
                     (-lambda ((&alist 'project-file project-file
                                 'default-repl-cmd default-repl-cmd))
                       (cond ((null project-file)
                              (funcall default-repl-cmd platform))
                             ((locate-file project-file
                                           (list root-dir))
                              (funcall default-repl-cmd platform))
                             (t nil)))
                     replique/clojure-build-tools))
         (repl-cmd (-> (-remove 'null repl-cmds)
                       car)))
    (or repl-cmd 'runnable-jar-not-found)))

(defun replique/comint-output-filter (proc string)
  (let* ((rdr (replique-edn/reader
               nil :str string))
         (msg (cond ((replique-edn/continuation-p
                     (car replique/tooling-handlers-queue))
                    (catch 'continuation
                      (replique-edn/contcall
                       (pop replique/tooling-handlers-queue)
                       rdr)))
                   ((s-starts-with?
                     "#ewen.replique.core.ToolingMsg"
                     (s-trim-left string))
                    (catch 'continuation
                      (replique-edn/read
                       rdr
                       replique/edn-tag-readers)))
                   (t nil))))
    (cond ((replique-edn/continuation-p msg)
           (push msg replique/tooling-handlers-queue))
          (msg (funcall
                (pop replique/tooling-handlers-queue)
                msg)
               (-let (((&alist 'type type
                        'error (&alist 'message err-msg))
                       msg)
                      (rest-str (replique-edn/reader-rest-string rdr)))
                 (cond (err-msg
                        (comint-output-filter proc err-msg)
                        (when rest-str
                          (replique/comint-output-filter
                           proc
                           rest-str)))

                       ((or (string= "load-file" type)
                            (string= "completions" type)
                            (string= "add-classpath" type))
                        (when rest-str
                          (->> rest-str
                               (s-chop-prefix "\n")
                               (replique/comint-output-filter proc))))
                       (t (when rest-str
                            (replique/comint-output-filter
                             proc
                             rest-str))))))
          (t (comint-output-filter proc string)))))

(defun replique/comint-output-filter-dispatch (proc string)
  (let ((s-list (s-slice-at "#ewen.replique.core.ToolingMsg" string)))
    (mapcar (-partial 'replique/comint-output-filter proc)
         s-list)))


(defun replique/repl* (root-dir platform repl-cmd)
  (when (not repl-cmd)
    (error "Clojure process cannot be started"))
  (if (equal repl-cmd 'runnable-jar-not-found)
      nil
    (let* ((default-directory root-dir)
           (buffer-name (replique/gen-buffer-name))
           (comint-buffer (apply #'make-comint
                                 buffer-name
                                 (car repl-cmd) nil (cdr repl-cmd))))
      (set-buffer comint-buffer)
      (replique/mode)
      (set-process-filter (replique/proc)
                          'replique/comint-output-filter-dispatch)
      (push `((name . ,buffer-name)
              (buffer . ,comint-buffer)
              (root-dir . ,root-dir)
              (platform . ,platform)
              (active . t)
              (sourcepaths . nil)
              (resourcepaths . nil))
            replique/buffers)
      (replique/set-active-buffer buffer-name)
      (pop-to-buffer comint-buffer)
      (when (not replique/buffers-hook-added)
        (add-hook
         'kill-buffer-hook
         (lambda ()
           (let ((b (current-buffer)))
             (setq replique/buffers
                   (-remove
                    (-lambda ((&alist 'buffer buffer))
                      (equal buffer b))
                    replique/buffers))
             (when (equal b (replique/get-active-buffer))
               (replique/set-active-buffer
                (->> (car replique/buffers)
                     (assoc 'name))))))))
      (setq replique/buffers-hook-added t))))

(defun replique/platform-to-name (platform)
  (cond ((equal platform "cljs")
         "Clojurescript")
        ((equal platform "clj")
         "Clojure")
        (t (error "Unknown platform: %s" platform))))

(defun replique/handle-jar-not-found (root-dir platform)
  (when (yes-or-no-p (format "Sorry, I could not find a %s jar on the filesystem in order to start the REPL. Would you like me to download it now?"
                             (replique/platform-to-name platform)))
    (-> (ido-read-directory-name
         (format "Please enter the directory where the %s jar should be saved: "
                 (replique/platform-to-name platform)))
        (replique-runnables/download-jar
         platform
         (lambda (jar-path)
           (replique/repl
            (replique/repl-cmd-raw jar-path platform)
            root-dir))))))

;;;###autoload
(defun replique/repl (&optional repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (ido-read-directory-name
                       "REPL root directory: "
                       (replique/guess-project-root-dir)))
            (repl-cmd (replique/project-repl-cmd root-dir "clj"))
            (repl-cmd (progn
                        (when (equal 'runnable-jar-not-found repl-cmd)
                          (replique/handle-jar-not-found root-dir "clj"))
                        repl-cmd)))
       (list repl-cmd root-dir))))
  (replique/repl* root-dir "clj" repl-cmd))

;;;###autoload
(defun replique/repl-cljs (&optional repl-cmd root-dir)
  "Run a Clojurescript REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (ido-read-directory-name
                       "REPL root directory: "
                       (replique/guess-project-root-dir)))
            (repl-cmd (replique/project-repl-cmd root-dir "cljs"))
            (repl-cmd (progn
                        (when (equal 'runnable-jar-not-found repl-cmd)
                          (replique/handle-jar-not-found root-dir "cljs"))
                        repl-cmd)))
       (list repl-cmd root-dir))))
  (replique/repl* root-dir "cljs" repl-cmd))


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
         (-lambda ((&alist 'name name
                     'buffer buffer
                     'active active
                     'root-dir root-dir
                     'platform platform
                     'sourcepaths sourcepaths
                     'resourcepaths resourcepaths))
           (if (equal buffer-name name)
               `((name . ,name)
                 (buffer . ,buffer)
                 (active . t)
                 (root-dir . ,root-dir)
                 (platform . ,platform)
                 (sourcepaths . ,sourcepaths)
                 (resourcepaths . ,resourcepaths))
             `((name . ,name)
               (buffer . ,buffer)
               (active . nil)
               (root-dir . ,root-dir)
               (platform . ,platform)
               (sourcepaths . ,sourcepaths)
               (resourcepaths . ,resourcepaths))))
         replique/buffers))
  (when display-msg
    (message "Active buffer switched to: %s" buffer-name)))

(defun replique/get-active-buffer-props (&optional error-on-nil)
  (let ((props (-first
                (-lambda ((&alist 'active active))
                  active)
                replique/buffers)))
    (if (and (null props) error-on-nil)
        (error "No Clojure subprocess")
      props)))

(defun replique/get-active-buffer (&optional error-on-nil)
  (->> (replique/get-active-buffer-props error-on-nil)
       (assoc 'buffer)
       cdr))

(defun replique/get-project-root-dir (&optional error-on-nil)
  (->> (replique/get-active-buffer-props error-on-nil)
       (assoc 'root-dir)
       cdr))

(defun replique/get-project-type (&optional error-on-nil)
  (let* ((root-dir (replique/get-project-root-dir error-on-nil))
         (project-props (-first
                         (-lambda ((&alist 'project-file project-file
                                     'default-repl-cmd default-repl-cmd))
                           (and
                            (not (null project-file))
                            (not (null (locate-file project-file
                                                    (list root-dir))))))
                         replique/clojure-build-tools)))
    (if project-props
        (cdr (assoc 'name project-props))
      "raw")))

(defun replique/get-project-platform (&optional error-on-nil)
  (->> (replique/get-active-buffer-props error-on-nil)
       (assoc 'platform)
       cdr))

(defun replique/get-in-project (prop &optional error-on-nil)
  (->> (replique/get-active-buffer-props error-on-nil)
       (assoc prop)
       cdr))


(defun replique/proc ()
  (get-buffer-process
   (if (derived-mode-p 'replique/mode)
       (current-buffer)
     (replique/get-active-buffer t))))



















(defun replique/eval-region (start end)
  (interactive "r")
  (let ((input (filter-buffer-substring start end)))
    (with-current-buffer
        (replique/get-active-buffer)
      (replique/comint-send-input-from-source input))))

(defun replique/eval-last-sexp ()
  "Send the previous sexp to the replique process."
  (interactive)
  (replique/eval-region (save-excursion (backward-sexp) (point)) (point)))

(defvar replique/prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `replique/load-file' command.")

(defun replique/load-file (file-name)
  "Load a Clojure file into the Clojure process."
  (interactive (comint-get-source "Load Clojure file: "
                                  replique/prev-l/c-dir/file
                                  '(clojure-mode)
                                  ;; nil because LOAD doesn't need
                                  ;; an exact name
                                  nil))
  ;; Check to see if buffer needs saved.
  (comint-check-source file-name)
  (setq replique/prev-l/c-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  (message "Loading Clojure file: %s ..." file-name)
  (replique/send-load-file file-name))

(defun replique/set-ns (ns)
  "Set the ns of the Clojure process.
Defaults to the ns of the current buffer."
  (interactive (replique/symprompt "Set ns to" (clojure-find-ns)))
  (message "Setting namespace to: %s ..." ns)
  (replique/send-set-ns ns))

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
      (progn
        (message "Adding sourcepath: %s ..." path)
        (replique/send-add-sourcepath path))))

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
    (progn
      (message "Adding resourcepath: %s ..." path)
      (replique/send-add-resourcepath path))))








(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" #'replique/eval-region)
    (define-key map "\C-x\C-e" #'replique/eval-last-sexp)
    (define-key map "\C-c\C-l" #'replique/load-file)
    (define-key map "\C-c\M-n" #'replique/set-ns)
    (easy-menu-define replique/minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique/eval-region t]
        ["Eval last sexp" replique/eval-last-sexp t]
        "--"
        ["Load file" replique/load-file t]
        "--"
        ["Set REPL ns" replique/set-ns t]))
    map))

;;;###autoload
(define-minor-mode replique/minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/minor-mode-map}"
  :lighter "Replique" :keymap replique/minor-mode-map
  (make-local-variable 'company-backends))








;; Tooling messages

(defun replique/handler-load-file (file-name msg)
  (-let (((&alist 'error err) msg))
    (if err
        (message "Loading Clojure file: %s ... Failed." file-name)
      (message "Loading Clojure file: %s ... Done." file-name))))

(defun replique/handler-set-ns (ns msg)
  (-let (((&alist 'error err) msg))
    (if err
        (message "Setting namespace to: %s ... Failed." ns)
      (message "Setting namespace to: %s ... Done." ns))))

(defun replique/handler-completions (callback msg)
  (-let (((&alist 'result result 'error err) msg))
    (when (not err)
      (funcall callback result))))

(defun replique/handler-add-sourcepath (buffer sourcepath msg)
  (-let (((&alist 'result result 'error err) msg))
    (if (and (not (null result))
             (not err))
        (let ((sourcepaths (assoc 'sourcepaths buffer)))
          (setcdr sourcepaths
                  (-union (cdr sourcepaths) (list sourcepath)))
          (message "Adding sourcepath: %s ... Done." sourcepath))
      (message "Adding sourcepath: %s ... Failed." sourcepath))))

(defun replique/handler-add-resourcepath (buffer resourcepath msg)
  (-let (((&alist 'result result 'error err) msg))
    (if (and (not (null result))
             (not err))
        (let ((resourcepaths (assoc 'resourcepaths buffer)))
          (setcdr resourcepaths
                  (-union (cdr resourcepaths) (list resourcepath)))
          (message "Adding sourcepath: %s ... Done." resourcepath))
      (message "Adding resourcepath: %s ... Failed." resourcepath))))

(defvar replique/tooling-handlers-queue '())

(defun replique/tooling-send-msg (msg callback)
  (let* ((proc (replique/proc))
         (msg (replique/alist-to-map msg)))
    (if replique/tooling-handlers-queue
        (nconc replique/tooling-handlers-queue (list callback))
      (setq replique/tooling-handlers-queue (list callback)))
    (->> (format "(ewen.replique.core/tooling-msg-handle %s)"
                 (replique-edn/pr-str msg))
         (funcall comint-input-sender proc))))

(defun replique/send-load-file (file-name)
  (-> `((:type . "load-file")
        (:file-path . ,file-name))
      (replique/tooling-send-msg
       (-partial 'replique/handler-load-file file-name))))

(defun replique/send-set-ns (ns)
  (-> `((:type . "set-ns")
        (:ns . ,ns))
      (replique/tooling-send-msg
       (-partial 'replique/handler-set-ns ns))))

(defun replique/send-completions (prefix company-callback)
  (-> `((:type . "completions")
        (:prefix . ,prefix))
      (replique/tooling-send-msg
       (-partial 'replique/handler-completions company-callback))))

(defun replique/send-add-sourcepath (path)
  (-> `((:type . "add-classpath")
        (:path . ,path))
      (replique/tooling-send-msg
       (-partial
        'replique/handler-add-sourcepath
        (replique/get-active-buffer-props t)
        path))))

(defun replique/send-add-resourcepath (path)
  (-> `((:type . "add-classpath")
        (:path . ,path))
      (replique/tooling-send-msg
       (-partial
        'replique/handler-add-resourcepath
        (replique/get-active-buffer-props t)
        path))))




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
