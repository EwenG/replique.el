;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.10.0") (company "0.8.12") (dash-functional "1.2.0") (s "1.9.0") (edn "1.1))
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'replique-edn)






(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/safe-s-split (separator s &optional omit-nulls)
  "Same as s-split but restore the global value of the match data"
  (save-match-data
    (s-split separator s omit-nulls)))

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


(defun replique/comint-input-sender (proc string)
  (comint-simple-send
   proc
   (replace-regexp-in-string "\n" "" string)))

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
  (setq comint-input-sender #'replique/comint-input-sender)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-to-list 'company-backends 'replique/company-backend))













(defun replique/repl-cmd-raw (jar platform)
  (let ((platform-suffix (if (string= "cljs" platform) "-cljs" "")))
    `("java" "-cp" ,jar "clojure.main" "-e"
      ,(format "(do (load-file \"%sclojure/ewen/replique/init%s.clj\") (ewen.replique.init/init \"%s\"))" (replique/replique-root-dir) platform-suffix (replique/replique-root-dir)))))

(defvar replique/clojure-build-tools
  (cl-flet ((platform-jar (platform)
                          (caar (replique/jars-in-path platform))))
            `((leiningen
               . ((project-file
                   . "project.clj")
                  (default-repl-cmd
                    . ,(lambda (platform)
                         `("lein" "run" "-m" "clojure.main/main" "-e"
                           ,(format "(do (load-file \"%sclojure/ewen/replique/init.clj\") (ewen.replique.init/init \"%s\"))" (replique/replique-root-dir) platform-suffix (replique/replique-root-dir)))))))
              (raw . ((project-file . nil)
                      (default-repl-cmd
                        . ,(lambda (platform)
                             (replique/repl-cmd-raw
                              (platform-jar platform)
                              platform)))))
              ;; (boot . ((project-file . "boot.clj")
              ;;          (default-repl-cmd . ,(lambda ()
              ;;                                 '("boot" "repl)"))))
              )))

(defun replique/build-files ()
  (delete nil (mapcar (lambda (project-props)
                        (cdr (assoc 'project-file (cdr project-props))))
                      replique/clojure-build-tools)))















(defconst replique/clj-jar-regex "clojure-\\([[:digit:].]+\\)-?\\(beta\\|alpha\\|RC\\|SNAPSHOT\\)?\\([0-9]+\\)?.jar$")
(defconst replique/cljs-jar-regex "clojurescript-\\([[:digit:].]+\\)-?\\([0-9]+\\)?-standalone.jar$")

(comment
 (replique/clj-jar->version  "clojure-1.6.0-beta45.jar")
 (replique/cljs-jar->version  "clojurescript-0.0-3308-standalone.jar")
 )


(defun replique/clj-jar->version (jar-name)
  (save-match-data
    (string-match replique/clj-jar-regex jar-name)
    (list jar-name
          (->> (or (match-string 1 jar-name) "0")
               (replique/safe-s-split "\\.")
               (-map 'string-to-number))
          (match-string 2 jar-name)
          (-> (or (match-string 3 jar-name) "0")
              string-to-number))))

(defun replique/cljs-jar->version (jar-name)
  (save-match-data
    (string-match replique/cljs-jar-regex jar-name)
    (list jar-name
          (->> (or (match-string 1 jar-name) "0")
               (replique/safe-s-split "\\.")
               (-map 'string-to-number))
          (-> (or (match-string 2 jar-name) "0")
              string-to-number))))

(comment
 (replique/release-version< '(4 6) '(3 5))
 )

(defun replique/release-version< (x y)
  (-let (((v1 . r1) x)
         ((v2 . r2) y))
    (cond ((not v1) t)
          ((not v2) nil)
          ((< v1 v2) t)
          ((> v1 v2) nil)
          (t (replique/release-version< r1 r2)))))

(defun replique/release-type< (x y)
  (cond ((equal y nil) t)
        ((and (equal x "SNAPSHOT") (equal y "RC")) t)
        ((and (equal x "SNAPSHOT") (equal y "alpha")) t)
        ((and (equal x "SNAPSHOT") (equal y "beta")) t)
        ((and (equal x "alpha") (equal y "RC")) t)
        ((and (equal x "alpha") (equal y "beta")) t)
        ((and (equal x "beta") (equal y "RC")) t)
        (t nil)))
(comment
 ("clojure-1.6.0-beta45.jar" (1 6 0) "beta" 0)
 ("clojurescript-0.0-3297.jar" (0 0) 3287)
 )

(defun replique/clj-jar-version<  (x y)
  (-let (((jar-name-1 v1 type-1 type-v-1) x)
         ((jar-name-2 v2 type-2 type-v-2) y))
    (cond ((and (equal v1 v2) (equal type-1 type-2))
           (< type-v-1 type-v-2))
          ((equal v1 v2)
           (replique/release-type< type-1 type-2))
          (t (replique/release-version< v1 v2)))))

(defun replique/cljs-jar-version<  (x y)
  (-let (((jar-name-1 v11 v12) x)
         ((jar-name-2 v21 v22) y))
    (cond ((and (equal v11 v21))
           (< v12 v22))
          (t (replique/release-version< v11 v21)))))

(defun replique/clj-jar-version> (x y)
  (not (replique/clj-jar-version< x y)))

(defun replique/cljs-jar-version> (x y)
  (not (replique/cljs-jar-version< x y)))

(comment
 (replique/jars-in-path "cljs")
 )

(defun replique/jars-in-path (platform)
  (-let* ((path (eshell-command-result "echo $PATH"))
          (path-dirs (split-string path ":"))
          (jar-regex (cond ((string= "clj" platform)
                            replique/clj-jar-regex)
                           ((string= "cljs" platform)
                            replique/cljs-jar-regex)
                           (t (error "Unsupported platform: %s"
                                     platform))))
          (version-parse-fn (cond ((string= "clj" platform)
                                   'replique/clj-jar->version)
                                  ((string= "cljs" platform)
                                   'replique/cljs-jar->version)
                                  (t (error "Unsupported platform: %s"
                                            platform))))
          (jar-version< (cond ((string= "clj" platform)
                               'replique/clj-jar-version<)
                              ((string= "cljs" platform)
                               'replique/cljs-jar-version<)
                              (t (error "Unsupported platform: %s"
                                        platform))))
          (jars (-> (-keep (lambda (dir)
                             (directory-files
                              dir t jar-regex))
                           path-dirs)
                    -flatten))
          (jars-versions (-map version-parse-fn jars)))
    (-sort jar-version< jars-versions)))



(defun replique/project-root-dir ()
  (or (car (remove
            nil
            (mapcar (lambda
                      (file)
                      (locate-dominating-file default-directory file))
                    (replique/build-files))))
      default-directory))

(defun replique/project-repl-cmd (root-dir platform)
  (let ((repl-cmds (mapcar
                    (-lambda ((build-tool-name . project-props))
                      (let ((project-file (-> (assoc
                                               'project-file
                                               project-props)
                                              cdr))
                            (default-repl-cmd (-> (assoc
                                                   'default-repl-cmd
                                                   project-props)
                                                  cdr)))
                        (cond ((null project-file)
                               (funcall default-repl-cmd platform))
                              ((locate-file project-file
                                            (list root-dir))
                               (funcall default-repl-cmd platform))
                              (t nil))))
                    replique/clojure-build-tools)))
    (-> (-remove 'null repl-cmds)
        car)))


(defun replique/comint-output-filter (proc string)
  (let ((msg (cond ((replique-edn/continuation-p
                     (car replique/tooling-handlers-queue))
                    (catch 'continuation
                      (replique-edn/contcall
                       (pop replique/tooling-handlers-queue)
                       (replique-edn/reader
                        nil :str string))))
                   ((s-starts-with?
                     "#ewen.replique.core.ToolingMsg"
                     (s-trim-left string))
                    (catch 'continuation
                      (replique-edn/read
                       (replique-edn/reader
                        nil :str string)
                       replique/edn-tag-readers)))
                   (t nil))))
    (cond ((replique-edn/continuation-p msg)
           (push msg replique/tooling-handlers-queue))
          (msg (funcall
                (pop replique/tooling-handlers-queue)
                msg)
               (-let (((&alist 'error (&alist 'message err-msg)) msg))
                 (when err-msg
                   (comint-output-filter proc (concat err-msg "\n")))))
          (t (comint-output-filter proc string)))))




(defun replique/compute-root-dir (&optional root-dir prefix-arg)
  (cond (root-dir root-dir)
        (prefix-arg
         (read-string
          "REPL process initial directory: "
          (replique/project-root-dir)))
        (t (replique/project-root-dir))))

(defun replique/compute-repl-cmd (root-dir platform &optional repl-cmd prefix-arg)
  (cond (repl-cmd repl-cmd)
        (current-prefix-arg
         (read-string
          "REPL launch command: "
          (replique/project-repl-cmd root-dir platform)))
        (t (replique/project-repl-cmd root-dir platform))))

(defun replique/repl* (root-dir repl-cmd)
  (when (not repl-cmd)
    (error "Clojure process cannot be started"))
  (let* ((default-directory root-dir)
         (buffer-name (replique/gen-buffer-name))
         (comint-buffer (apply #'make-comint
                               buffer-name
                               (car repl-cmd) nil (cdr repl-cmd))))
    (set-buffer comint-buffer)
    (replique/mode)
    (set-process-filter (replique/proc) 'replique/comint-output-filter)
    (push `((name . ,buffer-name)
            (buffer . ,comint-buffer)
            (active . t))
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
    (setq replique/buffers-hook-added t)))

;;;###autoload
(defun replique/repl (&optional repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (replique/compute-root-dir))
            (repl-cmd (replique/compute-repl-cmd root-dir "clj")))
       (list repl-cmd root-dir))))
  (replique/repl* root-dir repl-cmd))

;;;###autoload
(defun replique/repl-cljs (&optional repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (replique/compute-root-dir))
            (repl-cmd (replique/compute-repl-cmd root-dir "cljs")))
       (list repl-cmd root-dir))))
  (replique/repl* root-dir repl-cmd))


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
                     'active active))
           (if (equal buffer-name name)
               `((name . ,name)
                 (buffer . ,buffer)
                 (active . t))
             `((name . ,name)
               (buffer . ,buffer)
               (active . nil))))
         replique/buffers))
  (when display-msg
    (message "Active buffer switched to: %s" buffer-name)))

(defun replique/get-active-buffer ()
  (->> (-first
       (-lambda ((&alist 'active active))
         active)
       replique/buffers)
       (assoc 'buffer)
       cdr))

(defun replique/proc ()
  (let ((proc (get-buffer-process
               (if (derived-mode-p 'replique/mode)
                   (current-buffer)
                 (replique/get-active-buffer)))))
    (or proc
        (error "No Clojure subprocess"))))



















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
      (progn
        (with-current-buffer
            (replique/get-active-buffer)
          (replique/comint-refresh-prompt))
        (message "Setting namespace to: %s ... Done." ns)))))

(defun replique/handler-completions (callback msg)
  (-let (((&alist 'result result 'error err) msg))
    (when (not err)
      (funcall callback result))))

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



(provide 'replique)

;;; replique.el ends here
