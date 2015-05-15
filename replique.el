;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((emacs "24") (clojure-mode "4.0.1") (dash "2.10.0") (dash-functional "1.2.0") (s "1.9.0"))
;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'comint)
(require 'clojure-mode)
(require 'edn)






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

(defun replique/comint-send-input-from-source (input &optional no-newline artificial)
  (interactive)
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

(comment
 (let ((msg (edn-read "#ewen.replique.init.ToolingMsg{:type \"load-file\" :param \"/dir/file.clj\" :result \"#'test-project.core/foo\"}"))
       (cdr (assoc 'type msg))))
 )

(edn-add-reader :ewen.replique.init.ToolingMsg
                (-lambda (msg)
                  `((type . ,(gethash :type msg))
                    (param . ,(gethash :param msg))
                    (result . ,(gethash :result msg)))))

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
  (clojure-font-lock-setup))














(defun replique/repl-cmd-raw (clojure-jar)
  `("java" "-cp" ,clojure-jar "clojure.main" "-e"
    ,(format "(load-file \"%sclojure/ewen/replique/init.clj\")"
             (replique/replique-root-dir))))

(defvar replique/clojure-build-tools
  (cl-flet ((clojure-jar () (caar (replique/clojure-jars-in-path))))
    `((leiningen . ((project-file . "project.clj")
                    (default-repl-cmd . ,(lambda ()
                                           `("lein" "run" "-m" "clojure.main/main" "-e"
                                             ,(format "(load-file \"%sclojure/ewen/replique/init.clj\")"
                                                      (replique/replique-root-dir)))))))
      ;; (boot . ((project-file . "boot.clj")
      ;;          (default-repl-cmd . ,(lambda ()
      ;;                                 '("boot" "repl)"))))
      (raw . ((default-repl-cmd . ,(lambda ()
                                     (replique/repl-cmd-raw (clojure-jar)))))))))

(defun replique/build-files ()
  (delete nil (mapcar (lambda (project-props)
                        (cdr (assoc 'project-file (cdr project-props))))
                      replique/clojure-build-tools)))















(defconst replique/clojure-jar-regex "clojure-\\([[:digit:].]+\\)-?\\(beta\\|alpha\\|SNAPSHOT\\)?\\([0-9]+\\)?.jar$")

(comment
 (replique/clojure-jar->version  "clojure-1.6.0-beta45.jar")
 )


(defun replique/clojure-jar->version (jar-name)
  (save-match-data
    (string-match replique/clojure-jar-regex jar-name)
    (list jar-name
          (->> (or (match-string 1 jar-name) "0")
               (replique/safe-s-split "\\.")
               (-map 'string-to-number))
          (match-string 2 jar-name)
          (-> (or (match-string 3 jar-name) "0")
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
  (cond ((equal x y) nil)
        ((equal x nil) nil)
        ((equal y nil) t)
        ((and (equal x "SNAPSHOT") (equal y "alpha")) t)
        ((and (equal x "SNAPSHOT") (equal y "beta")) t)
        ((and (equal x "alpha") (equal y "beta")) t)
        ((and (equal y "SNAPSHOT") (equal x "alpha")) nil)
        ((and (equal y "SNAPSHOT") (equal x "beta")) nil)
        ((and (equal y "alpha") (equal x "beta")) nil)
        (t nil)))
(comment
 ("clojure-1.6.0-beta45.jar" (1 6 0) "beta" 0)
 )

(defun replique/jar-version<  (x y)
  (-let (((jar-name-1 v1 type-1 type-v-1) x)
         ((jar-name-2 v2 type-2 type-v-2) y))
    (cond ((and (equal v1 v2) (equal type-1 type-2))
           (< type-v-1 type-v-2))
          ((equal v1 v2)
           (replique/release-type< type-1 type-2))
          (t (replique/release-version< v1 v2)))))

(defun replique/jar-version> (x y)
  (not (replique/jar-version< x y)))

(comment
 (replique/clojure-jars-in-path)
 )

(defun replique/clojure-jars-in-path ()
  (-let* ((path (eshell-command-result "echo $PATH"))
          (path-dirs (split-string path ":"))
          (clojure-jars (-> (-keep (lambda (dir)
                                     (directory-files
                                      dir t replique/clojure-jar-regex))
                                   path-dirs)
                            -flatten))
          (clojure-jar-versions (-map 'replique/clojure-jar->version
                                      clojure-jars)))
    (-sort 'replique/jar-version< clojure-jar-versions)))



(defun replique/project-root-dir ()
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           (replique/build-files))))
      default-directory))

(defun replique/project-repl-cmd (root-dir)
  (or (car
       (delete nil
               (mapcar (lambda (project-props)
                         (when (locate-file (or (cdr (assoc 'project-file (cdr project-props))) "")
                                            (list root-dir))
                           (funcall (cdr (assoc 'default-repl-cmd (cdr project-props))))))
                       replique/clojure-build-tools)))
      (funcall (assoc-recursive replique/clojure-build-tools 'raw 'default-repl-cmd))))





(defun replique/comint-output-filter (proc string)
  (cond
   ;; Tooling output messages
   ((s-starts-with? "#ewen.replique.init.ToolingMsg" string)
    (-let* ((msg (edn-read string))
            (type (-> (assoc 'type msg)
                      cdr))
            (handler (-> (assoc type replique/tooling-handlers)
                         cdr)))
      (funcall handler msg)))
   ;; All other outputs
   (t (comint-output-filter proc string))))

;;;###autoload
(defun replique/repl (repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (cond ((boundp 'root-dir) root-dir)
                            (current-prefix-arg (read-string "REPL process initial directory: " (replique/project-root-dir)))
                            (t (replique/project-root-dir))))
            (repl-cmd (cond ((boundp 'repl-cmd) repl-cmd)
                            (current-prefix-arg (read-string "REPL launch command: " (replique/project-repl-cmd root-dir)))
                            (t (replique/project-repl-cmd root-dir)))))
       (list repl-cmd root-dir))))
  (if (not (comint-check-proc "*replique*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory root-dir))
        (set-buffer (apply #'make-comint
                           "replique" (car repl-cmd) nil (cdr repl-cmd)))
        (replique/mode)))
  (set-process-filter (replique/proc) 'replique/comint-output-filter)
  (setq replique/buffer "*replique*")
  (pop-to-buffer "*replique*"))

(defvar replique/buffer nil)

(defun replique/proc ()
  (let ((proc (get-buffer-process (if (derived-mode-p 'replique/mode)
                                      (current-buffer)
                                    replique/buffer))))
    (or proc
        (error "No Clojure subprocess; see variable `replique/buffer'"))))


















(defun replique/eval-region (start end)
  (interactive "r")
  (let ((input (filter-buffer-substring start end)))
    (with-current-buffer
        replique/buffer
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










(defvar replique/minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r" #'replique/eval-region)
    (define-key map "\C-x\C-e" #'replique/eval-last-sexp)
    (define-key map "\C-c\C-e" #'replique/eval-last-sexp)
    (define-key map "\C-c\C-l" #'replique/load-file)
    (easy-menu-define replique/minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique/eval-region t]
        ["Eval last sexp" replique/eval-last-sexp t]
        "--"
        ["Load file" replique/load-file t]))
    map))

;;;###autoload
(define-minor-mode replique/minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique/minor-mode-map}"
  :lighter "Replique" :keymap replique/minor-mode-map)








;; Tooling messages

(defun replique/handler-load-file (msg)
  (print msg)
  (-let ((((type . type)
           (param . file-name)
           (result . result)) msg))
    (message "Loading Clojure file: %s ... Done." file-name)))

(defvar replique/tooling-handlers
  `(("load-file" . replique/handler-load-file)))

(defun replique/tooling-msg (type param result)
  (format "(ewen.replique.init.ToolingMsg. \"%s\" \"%s\" %s)"
          type param result))

(defun replique/send-load-file (file-name)
  (let ((proc (replique/proc)))
    (funcall
     comint-input-sender
     proc
     (replique/tooling-msg
      "load-file"
      file-name
      (format "(str (clojure.core/load-file \"%s\"))"
              file-name)))))







(provide 'replique)

;;; replique.el ends here
