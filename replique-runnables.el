;;; replique-runnables.el ---   -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 's)

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)

(defun replique-runnables/safe-s-split (separator s &optional omit-nulls)
  "Same as s-split but restore the global value of the match data"
  (save-match-data
    (s-split separator s omit-nulls)))




(defcustom replique-runnables/clj-url "https://repo1.maven.org/maven2/org/clojure/clojure/1.7.0/clojure-1.7.0.jar"
  "The URL to use when dowloading the Clojure jar."
  :type 'string
  :group 'replique)

(defcustom replique-runnables/clj-dep [org.clojure/clojure "1.7.0"]
  "The default Clojure dependency."
  :type 'string
  :group 'replique)

(defcustom replique-runnables/cljs-url "https://github.com/clojure/clojurescript/releases/download/r1.7.48/cljs.jar"
  "The URL to use when dowloading the Clojurescriptjar."
  :type 'string
  :group 'replique)

(defvar replique-runnables/cljs-file-name "clojurescript-1.7.48-standalone.jar")

(defcustom replique-runnables/cljs-dep
  [org.clojure/clojurescript "1.7.48"]
  "The default Clojurescript dependency."
  :type 'string
  :group 'replique)


(defconst replique-runnables/clj-jar-regex "clojure-\\([[:digit:].]+\\)-?\\(beta\\|alpha\\|RC\\|SNAPSHOT\\)?\\([0-9]+\\)?.jar$")
(defconst replique-runnables/cljs-jar-regex "clojurescript-\\([[:digit:].]+\\)-standalone.jar$")

(defun replique-runnables/default-dep (platform)
  (cond ((string= platform "clj")
         replique-runnables/clj-dep)
        ((string= platform "cljs")
         replique-runnables/cljs-dep)
        (t (error "Unsupported platform: %s" platform))))

(defun replique-runnables/lein-path (replique-dir)
  (cond ((->> (eshell-command-result "which lein")
              (replace-regexp-in-string "\n$" "")
              file-exists-p)
         "lein")
        ((file-exists-p (concat replique-dir "runnables/lein"))
         (concat replique-dir "runnables/lein"))
        (t nil)))













;; Jar downloading

(defun replique-runnables/url-retrieve (url callback)
  (url-retrieve
   url
   (lambda (status)
     (cond ((null status)
            (funcall callback status))
           ((not (null (plist-get status :redirect)))
            (kill-buffer)
            (sleep-for 1)
            (replique-runnables/url-retrieve
             (plist-get status :redirect)
             callback))
           (t (error "Error while downloading %s" status))))))

(defun replique-runnables/remove-http-header ()
  (beginning-of-buffer)
  (let ((beg (point)))
    (search-forward-regexp "^$")
    (forward-char)
    (delete-region beg (point))))

(defun replique-runnables/platform-jar-path (replique-dir platform)
  (cond ((string= "clj" platform)
         (concat replique-dir "runnables/clojure.jar"))
        ((string= "cljs" platform)
         (concat replique-dir "runnables/clojurescript.jar"))
        (t (error (format "Unsupported platform: %s"
                          platform)))))

(defun replique-runnables/download-jar (replique-dir platform callback)
  (let* ((url (cond ((equal platform "clj") replique-runnables/clj-url)
                    ((equal platform "cljs") replique-runnables/cljs-url)
                    (t (error "Unsupported platform: %s" platform))))
         (jar-path (concat
                    replique-dir
                    "runnables/"
                    (cond ((equal platform "clj")
                           (url-file-nondirectory url))
                          ((equal platform "cljs")
                           replique-runnables/cljs-file-name)
                          (t (error "Unsupported platform: %s"
                                    platform))))))
    (replique-runnables/url-retrieve
     url
     (lambda (status)
       (replique-runnables/remove-http-header)
       (ignore-errors
         (write-file jar-path t))
       (kill-buffer)
       (eshell-command-result
        (format "ln -s %s %s"
                jar-path
                (replique-runnables/platform-jar-path
                 replique-dir platform)))
       (funcall callback (replique-runnables/platform-jar-path
                          replique-dir platform))))
    jar-path))


(provide 'replique-runnables)

;;; replique-runnables.el ends here
