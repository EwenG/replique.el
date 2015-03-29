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


(let ((compare-release-type (lambda (x y)
                              ))
      (clojure-jars (split-string
                     (shell-command-to-string
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
")
                     " ")))
  (print (save-match-data
           (mapcar (lambda (x)
                     (string-match "clojure-\\([[:digit:].]+\\)-?\\(beta[0-9]*\\|alpha[0-9]*\\|SNAPSHOT\\)?.jar" x)
                     (list (match-string 1 x) (match-string 2 x)))
                   clojure-jars))))




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
