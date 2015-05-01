;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'replique)



(ert-deftest test-replique/clojure-jar->version ()
  (setq-local jar-1 "/home/user-name/bin/clojure-1.6.00-alpha45.jar")
  (should (equal (replique/clojure-jar->version jar-1)
                 `(,jar-1 (1 6 0) "alpha" 45))))

(ert-deftest test-replique/sort-jars ()
  (setq-local clojure-jars-1 '("/home/user-name/bin/clojure-1.6.00-alpha45.jar"
                               "/home/user-name/bin/clojure-1.6.00.jar"
                               "/home/user-name/bin/clojure-1.2-beta.jar"
                               "/home/user-name/bin/clojure-1.2.2-SNAPSHOT.jar"
                               "/home/user-name/bin/clojure-1.6.0.jar"))
  (setq-local clojure-jar-versions-1
              (-map 'replique/clojure-jar->version clojure-jars-1))
  (setq-local sorted-clojure-jar-versions-1
              (-sort 'replique/jar-version> clojure-jar-versions-1))
  (should (equal clojure-jar-versions-1
                 (list '("/home/user-name/bin/clojure-1.6.00-alpha45.jar" (1 6 0) "alpha" 45)
                       '("/home/user-name/bin/clojure-1.6.00.jar" (1 6 0) nil 0)
                       '("/home/user-name/bin/clojure-1.2-beta.jar" (1 2) "beta" 0)
                       '("/home/user-name/bin/clojure-1.2.2-SNAPSHOT.jar" (1 2 2) "SNAPSHOT" 0)
                       '("/home/user-name/bin/clojure-1.6.0.jar" (1 6 0) nil 0))))
  (should (equal sorted-clojure-jar-versions-1
                 (list '("/home/user-name/bin/clojure-1.6.0.jar" (1 6 0) nil 0)
                       '("/home/user-name/bin/clojure-1.6.00.jar" (1 6 0) nil 0)
                       '("/home/user-name/bin/clojure-1.6.00-alpha45.jar" (1 6 0) "alpha" 45)
                       '("/home/user-name/bin/clojure-1.2.2-SNAPSHOT.jar" (1 2 2) "SNAPSHOT" 0)
                       '("/home/user-name/bin/clojure-1.2-beta.jar" (1 2) "beta" 0)))))
