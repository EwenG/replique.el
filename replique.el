;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'comint)
(require 'clojure-mode)











(defun ht-map (function table)
  "Apply FUNCTION to each key-value pair of TABLE, and make a list of the results.
FUNCTION is called with two arguments, KEY and VALUE.
Taken from ht.el."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall function key value) results))
     table)
    results))












(defgroup replique nil
  ""
  :group 'clojure)













(defcustom replique-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)







(defvar replique-mode-hook '()
  "Hook for customizing replique mode.")

(define-derived-mode replique-mode comint-mode "Replique"
  ""
  (setq comint-prompt-regexp inf-clojure-prompt)
  (setq mode-line-process '(":%s")))














(defun replique-repl-cmd-raw (clojure-jar)
  (concat "java -cp " clojure-jar " clojure.main"))

(cl-flet ((clojure-jar () (car (first (replique-clojure-jars-in-path)))))
  (defvar replique-clojure-build-tools
    #s(hash-table test equal data
                  ("leiningen" #s(hash-table test equal
                                              data ("project-file" "project.clj"
                                                    "default-repl-cmd" (lambda () "lein repl")))
                   "boot" #s(hash-table test equal
                                         data ("project-file" "boot.build"
                                               "default-repl-cmd" (lambda () "boot repl")))
                   "raw" #s(hash-table test equal
                                        data ()))))
  (puthash "default-repl-cmd" (lambda ()
                               (replique-repl-cmd-raw (clojure-jar)))
           (gethash "raw" replique-clojure-build-tools)))

(defun replique-build-files ()
  (delete nil (ht-map (lambda (k build-tool-map)
                        (gethash "project-file" build-tool-map))
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
               (ht-map (lambda (k build-tool-map)
                         (when (locate-file (or (gethash "project-file" build-tool-map) "")
                                            (list root-dir))
                           (funcall (gethash "default-repl-cmd" build-tool-map))))
                       replique-clojure-build-tools)))
      (funcall (gethash "default-repl-cmd" (gethash "raw" replique-clojure-build-tools)))))







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
  (comint-send-region (replique-proc) start end)
  (comint-send-string (replique-proc) "\n"))


















(provide 'replique)

;;; replique.el ends here
