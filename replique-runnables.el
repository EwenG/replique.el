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

(defcustom replique-runnables/cljs-url "https://github.com/clojure/clojurescript/releases/download/r1.7.170/cljs.jar"
  "The URL to use when dowloading the Clojurescriptjar."
  :type 'string
  :group 'replique)

(defvar replique-runnables/cljs-file-name "clojurescript-1.7.170-standalone.jar")

(defcustom replique-runnables/cljs-dep
  [org.clojure/clojurescript "1.7.170"]
  "The default Clojurescript dependency."
  :type 'string
  :group 'replique)

(defcustom replique-runnables/sass-url "https://github.com/EwenG/replique.el/releases/download/0.0.1/replique_sass_3.2.5_0.0.1"
  "The URL to use when dowloading the replique-sass binary."
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

(defun replique-runnables/platform-jar-path (replique-dir platform)
  (cond ((string= "clj" platform)
         (concat replique-dir "runnables/clojure.jar"))
        ((string= "cljs" platform)
         (concat replique-dir "runnables/clojurescript.jar"))
        (t (error (format "Unsupported platform: %s"
                          platform)))))

(defun replique-runnables/replique-sass-path (replique-dir)
  (let ((sass-path (concat replique-dir "runnables/replique_sass")))
    (if (file-exists-p sass-path)
        sass-path
      nil)))













;; Jar downloading

(defun replique-runnables/parse-http-headers (buffer)
  (with-current-buffer buffer
    (cond ((> (buffer-size buffer) 0)
           (beginning-of-buffer)
           (let ((beg (point)))
             (search-forward-regexp "^$")
             (forward-char)
             (save-match-data
               (let* ((header (buffer-substring-no-properties beg (point)))
                      (length (if header
                                  (string-match
                                   "^Content-Length:\\(.+\\)$" header)))
                      (length (if length
                                  (match-string-no-properties 1 header)))
                      (location (if header
                                    (string-match
                                     "^Location:\\(.+\\)$" header)))
                      (location (if location
                                    (match-string-no-properties
                                     1 header))))
                 (list (if length
                           (string-to-number (string-trim length))
                         nil)
                       (if location
                           (string-trim location)
                         nil))))))
          (t '(nil nil)))))

(defun replique-runnables/get-http-buffer-length (buffer)
  (with-current-buffer buffer
    (end-of-buffer)
    (point)))

(defun replique-runnables/remove-http-header ()
  (beginning-of-buffer)
  (let ((beg (point)))
    (search-forward-regexp "^$")
    (forward-char)
    (delete-region beg (point))))

(defun replique-runnables/download-ratio (length content-length)
  (format "%d%s"
          (truncate (* (/ (float length) (float content-length)) 100))
          "%"))

(defun replique-runnables/print-download-progress (buffer url)
  (let ((content-length
         (car (replique-runnables/parse-http-headers buffer)))
        (length
         (replique-runnables/get-http-buffer-length buffer)))
    (if content-length
        (message "Downloading %s ... %s"
                 url
                 (replique-runnables/download-ratio
                  length content-length))
      (message "Downloading %s ... %s" url "0%"))))

(defun replique-runnables/handle-redirects (buffer)
  (cadr (replique-runnables/parse-http-headers buffer)))

(defun replique-runnables/url-retrieve (url done-fn)
  (let* ((timer nil)
         (download-buffer
         (url-retrieve
          url
          (lambda (status)
            (cancel-timer timer)
            (cond ((null status)
                   (funcall done-fn status))
                  ((not (null (plist-get status :redirect)))

                   ;; We would normally handle redirects here.
                   ;; But the status code is sometimes very long to
                   ;; receive! (no idea why).
                   ;; So redirects are handled by reading the "Location"
                   ;; entry of HTTP headers instead.

                   ;; (kill-buffer)
                   ;; (sleep-for 1)
                   ;; (replique-runnables/url-retrieve
                   ;;  (plist-get status :redirect)
                   ;;  done-fn)
                   )
                  (t
                   (kill-buffer)
                   (error "Error while downloading %s" status))))
          nil t)))
    (setq timer (run-at-time
                 0 2
                 (lambda (buffer)
                   (if (not (buffer-live-p buffer))
                       (cancel-timer timer)
                     (let ((location
                            (replique-runnables/handle-redirects buffer)))
                       (if location
                           (progn
                             (cancel-timer timer)
                             (kill-buffer buffer)
                             (replique-runnables/url-retrieve
                              location done-fn))
                         (replique-runnables/print-download-progress
                          buffer url)))))
                 download-buffer))))

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
                                    platform)))))
         (done-fn (lambda (status)
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
                                       replique-dir platform)))))
    (replique-runnables/url-retrieve url done-fn)
    jar-path))

(defun replique-runnables/download-sass (replique-dir callback)
  (let* ((sass-path (concat
                     replique-dir
                     "runnables/"
                     (url-file-nondirectory
                      replique-runnables/sass-url)))
         (sym-link-path (concat replique-dir "runnables/replique_sass"))
         (download-buffer
          (replique-runnables/url-retrieve
           replique-runnables/sass-url
           (lambda (status)
             (replique-runnables/remove-http-header)
             (ignore-errors
               (write-file sass-path t))
             (kill-buffer)
             (eshell-command (concat "chmod +x " sass-path))
             (eshell-command
              (format "ln -s %s %s" sass-path sym-link-path))
             (funcall callback sass-path)))))
    (replique-runnables/print-download-progress
     download-buffer replique-runnables/sass-url)
    sass-path))

(provide 'replique-runnables)

;;; replique-runnables.el ends here
