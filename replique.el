;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Package-Requires: ((dash "2.10.0") (dash-functional "1.2.0") (emacs "24"))
;;; Commentary:

;;; Code:

(require 'dash)
(require 's)
(require 'comint)
(require 'replique-comint)
(require 'clojure-mode)






(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique/safe-s-split (separator s &optional omit-nulls)
  (save-match-data
    (s-split separator s omit-nulls)))









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

(defvar replique/clojure-build-tools
  (cl-flet ((clojure-jar () (caar (replique/clojure-jars-in-path))))
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
                       replique/clojure-build-tools)))
      (funcall (assoc-recursive replique/clojure-build-tools 'raw 'default-repl-cmd))))







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
