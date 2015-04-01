;;; replique.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'comint)
(require 'clojure-mode)

(defgroup replique nil
  ""
  :group 'clojure)





(defcustom inf-clojure-prompt "^[^=> \n]+=> *"
  "Regexp to recognize prompts in the replique mode."
  :type 'regexp
  :group 'replique)







(defvar replique-mode-hook '()
  "Hook for customizing replique mode.")

(define-derived-mode replique-mode comint-mode "Replique"
  ""
  (setq comint-prompt-regexp inf-clojure-prompt)
  (setq mode-line-process '(":%s")))



(defvar replique-project-root-files
  '("project.clj" "build.boot")
  "A list of files that can be considered project markers.")

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
         (clojure-jar-versions (save-match-data
                                 (mapcar (lambda (x)
                                           (string-match "clojure-\\([[:digit:].]+\\)-?\\(beta\\|alpha\\|SNAPSHOT\\)?\\([0-9]+\\)?.jar" x)
                                           (list (or (match-string 1 x) "0") (match-string 2 x) (or (match-string 3 x) "0")))
                                         clojure-jars))))
    (sort clojure-jar-versions 'replique-jar-version>)))





(defun replique-project-root ()
  "Retrieve the root directory of a project if available.

Fallback to `default-directory.' if not within a project."
  (or (car (remove nil
                   (mapcar (lambda
                             (file)
                             (locate-dominating-file default-directory file))
                           inf-clojure-project-root-files)))
      default-directory))


;;;###autoload
(defun replique-repl (cmd)
  "Run an Clojure REPL, input and output via buffer `*replique*'."
  (interactive)
  (if (not (comint-check-proc "*replique*"))
      ;; run the new process in the project's root when in a project folder
      (let ((default-directory (replique-project-root))
            (cmdlist (split-string cmd)))
        (set-buffer (apply #'make-comint
                           "replique" (car cmdlist) nil (cdr cmdlist)))
        (replique-mode)))
  (setq replique-buffer "*replique*")
  (pop-to-buffer-same-window "*replique*"))







(provide 'replique)

;;; replique.el ends here
