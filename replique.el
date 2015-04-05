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



(defun replique-repl-cmd-raw (clojure-jar)
  (concat "java -cp " clojure-jar " clojure.main"))

(defvar replique-clojure-build-tools
  (list #s(hash-table test equal
                      data ("name" "leiningen"
                            "project-file" "project.clj"
                            "default-repl-cmd" (lambda () "lein repl")))
        #s(hash-table test equal
                      data ("name" "boot"
                            "project-file" "boot.build"
                            "default-repl-cmd" (lambda () "boot repl")))
        #s(hash-table test equal
                      data ("name" "raw"
                            "default-repl-cmd" (replique-repl-cmd-raw )))))




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
                           replique-project-root-files)))
      default-directory))




(defun replique-project-repl-cmd (project-root-dir)
  (print (directory-files project-root-dir "project.clj")))

;;;###autoload
(defun replique-repl (repl-cmd root-dir)
  "Run a Clojure REPL, input and output via buffer `*replique*'."
  (interactive ;; (list (if current-prefix-arg
               ;;           (read-string "Run Clojure: " replique-repl-cmd)
   ;;         replique-repl-cmd))
   (progn
     (let* ((root-dir (if (boundp 'root-dir)
                          root-dir
                        (replique-project-root-dir)))
            (repl-cmd (if (boundp 'repl-cmd)
                          repl-cmd
                        (replique-project-repl-cmd root-dir))))
       (list repl-cmd root-dir))))
  (prin1 (concat "prefix " current-prefix-arg)) (print "")
  (prin1 "root-dir ") (prin1 root-dir) (print "")
  (prin1 "repl-cmd ") (prin1 repl-cmd))







(provide 'replique)

;;; replique.el ends here
