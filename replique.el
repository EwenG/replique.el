;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'comint)
(require 'replique-comint)
(require 'clojure-mode)









(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)











(defgroup replique nil
  ""
  :group 'clojure)













(defcustom replique-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)

















(defun replique-comint-input-sender (proc string)
  (comint-simple-send proc
   (replace-regexp-in-string "\n" "" string)))

(defvar replique-mode-hook '()
  "Hook for customizing replique mode.")

(defvar replique-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-m" #'replique-comint-send-input)
    (define-key map "\C-x\C-e" #'replique-eval-last-sexp)
    map))


(define-derived-mode replique-mode comint-mode "Replique"
  "Commands:\\<replique-mode-map>
\\[replique-comint-send-input] after the end of the process' output sends the text from the
    end of process to point."
  (setq comint-prompt-regexp replique-prompt)
  (setq comint-read-only t)
  (setq comint-input-sender #'replique-comint-input-sender)
  (setq mode-line-process '(":%s"))
  (clojure-mode-variables)
  (clojure-font-lock-setup)
  (add-hook 'paredit-mode-hook #'clojure-paredit-setup))














(defun replique-repl-cmd-raw (clojure-jar)
  (concat "java -cp " clojure-jar " clojure.main"))

(cl-flet ((clojure-jar () (car (first (replique-clojure-jars-in-path)))))
  (defvar replique-clojure-build-tools
    `((leiningen . ((project-file . "project.clj")
                    (default-repl-cmd . ,(lambda ()
                                          "lein run -m clojure.main/main"))))
      (boot . ((project-file . "boot.clj")
               (default-repl-cmd . ,(lambda ()
                                     "boot repl"))))
      (raw . ((default-repl-cmd . ,(lambda ()
                                    (replique-repl-cmd-raw (clojure-jar)))))))))

(defun replique-build-files ()
  (delete nil (mapcar (lambda (project-props)
                        (cdr (assoc 'project-file (cdr project-props))))
                      replique-clojure-build-tools)))

















(defun replique-clojure-jar->version (jar-file)
  (save-match-data
    (string-match "clojure-\\([[:digit:].]+\\)-?\\(beta\\|alpha\\|SNAPSHOT\\)?\\([0-9]+\\)?.jar" jar-file)
    (cons jar-file (list (or (match-string 1 jar-file) "0")
                         (match-string 2 jar-file)
                         (or (match-string 3 jar-file) "0")))))

(defun replique-normalize-version-strings (x y)
  (let* ((diff-length (- (length x) (length y)))
         (abs-diff-length (abs diff-length)))
    (cond ((equal 0 diff-length) (list x y))
          ((< diff-length 0) (list (concat x (make-string abs-diff-length ?0)) y))
          ((> diff-length 0) (list x (concat y (make-string abs-diff-length ?0)))))))

(defun replique-release-type< (x y)
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

(defun replique-jar-version<  (x y)
  (let ((normalized-versions (replique-normalize-version-strings (car x) (car y))))
    (cond ((and (equal (car normalized-versions) (cadr normalized-versions))
                (equal (nth 1 x) (nth 1 y)))
           (string< (nth 2 x) (nth 2 y)))
          ((equal (car normalized-versions) (cadr normalized-versions))
           (replique-release-type< (nth 1 x) (nth 1 y)))
          (t (string< (car x) (car y))))))

(defun replique-jar-version> (x y)
  (not (replique-jar-version< x y)))

(defun replique-cmd-out->list (c-out)
  (if (> (length c-out) 0)
      (split-string
       c-out
       " ")
    nil))

(defun replique-clojure-jars-in-path ()
  (let* ((shell-cmd-out (shell-command-to-string
"for dir in `echo $PATH | sed \"s/:/ /g\"`
do
    if [ -d \"$dir\" ];
    then JARS=`find $dir -maxdepth 1 -name \"clojure-*.jar\"`;
    else JARS=\"\";
    fi
    #Exclude empty strings
    if [ \"x$JARS\" != \"x\" ];
    then echo -n $JARS;
    fi
done
"))
         (clojure-jars (replique-cmd-out->list shell-cmd-out))
         (clojure-jar-versions (mapcar 'replique-clojure-jar->version clojure-jars)))
    (sort clojure-jar-versions 'replique-jar-version>)))





(defun replique-project-root-dir ()
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           (replique-build-files))))
      default-directory))

(defun replique-project-repl-cmd (root-dir)
  (or (car
       (delete nil
               (mapcar (lambda (project-props)
                         (when (locate-file (or (cdr (assoc 'project-file (cdr project-props))) "")
                                            (list root-dir))
                           (funcall (cdr (assoc 'default-repl-cmd (cdr project-props))))))
                       replique-clojure-build-tools)))
      (funcall (assoc-recursive replique-clojure-build-tools 'raw 'default-repl-cmd))))







;;;###autoload
(defun replique-repl (repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive
   (progn
     (let* ((root-dir (cond ((boundp 'root-dir) root-dir)
                            (current-prefix-arg (read-string "REPL process initial directory: " (replique-project-root-dir)))
                            (t (replique-project-root-dir))))
            (repl-cmd (cond ((boundp 'repl-cmd) repl-cmd)
                            (current-prefix-arg (read-string "REPL launch command: " (replique-project-repl-cmd root-dir)))
                            (t (replique-project-repl-cmd root-dir)))))
       (list repl-cmd root-dir))))
  (if (not (comint-check-proc "*replique*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory root-dir)
            (repl-cmd (split-string repl-cmd)))
        (set-buffer (apply #'make-comint
                           "replique" (car repl-cmd) nil (cdr repl-cmd)))
        (replique-mode)))
  (setq replique-buffer "*replique*")
  (pop-to-buffer-same-window "*replique*"))

(defvar replique-buffer nil)

(defun replique-proc ()
  (let ((proc (get-buffer-process (if (derived-mode-p 'replique-mode)
                                      (current-buffer)
                                    replique-buffer))))
    (or proc
        (error "No Clojure subprocess; see variable `replique-buffer'"))))


















(defun replique-eval-region (start end)
  (interactive "r")
  (let ((input (filter-buffer-substring start end)))
    (with-current-buffer
        replique-buffer
      (replique-comint-send-input-from-source input))))

(defun replique-eval-last-sexp ()
  "Send the previous sexp to the replique process."
  (interactive)
  (replique-eval-region (save-excursion (backward-sexp) (point)) (point)))












(defvar replique-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-e" #'replique-eval-last-sexp)
    (easy-menu-define replique-minor-mode-menu map
      "Replique Minor Mode Menu"
      '("Replique"
        ["Eval region" replique-eval-region t]
        ["Eval last sexp" replique-eval-last-sexp t]))
    map))

;;;###autoload
(define-minor-mode replique-minor-mode
  "Minor mode for interacting with the replique process buffer.

The following commands are available:

\\{replique-minor-mode-map}"
  :lighter "Replique" :keymap replique-minor-mode-map)












(provide 'replique)

;;; replique.el ends here
