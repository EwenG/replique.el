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






(defconst replique-runnables/clj-jar-regex "clojure-\\([[:digit:].]+\\)-?\\(beta\\|alpha\\|RC\\|SNAPSHOT\\)?\\([0-9]+\\)?.jar$")
(defconst replique-runnables/cljs-jar-regex "clojurescript-\\([[:digit:].]+\\)-?\\([0-9]+\\)?-standalone.jar$")

(comment
 (replique-runnables/clj-jar->version  "clojure-1.6.0-beta45.jar")
 (replique-runnables/cljs-jar->version  "clojurescript-0.0-3308-standalone.jar")
 )


(defun replique-runnables/clj-jar->version (jar-name)
  (save-match-data
    (string-match replique-runnables/clj-jar-regex jar-name)
    (list jar-name
          (->> (or (match-string 1 jar-name) "0")
               (replique-runnables/safe-s-split "\\.")
               (-map 'string-to-number))
          (match-string 2 jar-name)
          (-> (or (match-string 3 jar-name) "0")
              string-to-number))))

(defun replique-runnables/cljs-jar->version (jar-name)
  (save-match-data
    (string-match replique-runnables/cljs-jar-regex jar-name)
    (list jar-name
          (->> (or (match-string 1 jar-name) "0")
               (replique-runnables/safe-s-split "\\.")
               (-map 'string-to-number))
          (-> (or (match-string 2 jar-name) "0")
              string-to-number))))

(comment
 (replique-runnables/release-version< '(4 6) '(3 5))
 )

(defun replique-runnables/release-version< (x y)
  (-let (((v1 . r1) x)
         ((v2 . r2) y))
    (cond ((not v1) t)
          ((not v2) nil)
          ((< v1 v2) t)
          ((> v1 v2) nil)
          (t (replique-runnables/release-version< r1 r2)))))

(defun replique-runnables/release-type< (x y)
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

(defun replique-runnables/clj-jar-version<  (x y)
  (-let (((jar-name-1 v1 type-1 type-v-1) x)
         ((jar-name-2 v2 type-2 type-v-2) y))
    (cond ((and (equal v1 v2) (equal type-1 type-2))
           (< type-v-1 type-v-2))
          ((equal v1 v2)
           (replique-runnables/release-type< type-1 type-2))
          (t (replique-runnables/release-version< v1 v2)))))

(defun replique-runnables/cljs-jar-version<  (x y)
  (-let (((jar-name-1 v11 v12) x)
         ((jar-name-2 v21 v22) y))
    (cond ((and (equal v11 v21))
           (< v12 v22))
          (t (replique-runnables/release-version< v11 v21)))))

(defun replique-runnables/clj-jar-version> (x y)
  (not (replique-runnables/clj-jar-version< x y)))

(defun replique-runnables/cljs-jar-version> (x y)
  (not (replique-runnables/cljs-jar-version< x y)))

(comment
 (replique-runnables/jars-in-path "cljs")
 )

(defun replique-runnables/jars-in-path (platform)
  (-let* ((path (eshell-command-result "echo $PATH"))
          (path-dirs (split-string path ":"))
          (jar-regex (cond ((string= "clj" platform)
                            replique-runnables/clj-jar-regex)
                           ((string= "cljs" platform)
                            replique-runnables/cljs-jar-regex)
                           (t (error "Unsupported platform: %s"
                                     platform))))
          (version-parse-fn (cond ((string= "clj" platform)
                                   'replique-runnables/clj-jar->version)
                                  ((string= "cljs" platform)
                                   'replique-runnables/cljs-jar->version)
                                  (t (error "Unsupported platform: %s"
                                            platform))))
          (jar-version< (cond ((string= "clj" platform)
                               'replique-runnables/clj-jar-version<)
                              ((string= "cljs" platform)
                               'replique-runnables/cljs-jar-version<)
                              (t (error "Unsupported platform: %s"
                                        platform))))
          (jars (-> (-keep (lambda (dir)
                             (directory-files
                              dir t jar-regex))
                           path-dirs)
                    -flatten))
          (jars-versions (-map version-parse-fn jars)))
    (-sort jar-version< jars-versions)))

(provide 'replique-runnables)

;;; replique-runnables.el ends here
