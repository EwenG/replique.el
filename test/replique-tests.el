;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'replique)



(ert-deftest test-replique-clojure-jar->version ()
  (setq-local jar-1 "/home/ewen/bin/clojure-1.6.00-alpha45.jar")
  (should (equal (replique-clojure-jar->version jar-1)
                 (cons jar-1 '("1.6.00" "alpha" "45")))))

(ert-deftest test-replique-sort-jars ()
  (setq-local clojure-jars-1 '("/home/ewen/bin/clojure-1.6.00-alpha45.jar" "/home/ewen/bin/clojure-1.6.00.jar" "/home/ewen/bin/clojure-1.2-beta.jar" "/home/ewen/bin/clojure-1.2.2-SNAPSHOT.jar" "/home/ewen/bin/clojure-1.6.0.jar"))
  (let ((clojure-jar-versions-1 (mapcar 'replique-clojure-jar->version clojure-jars-1)))
    (should (equal clojure-jar-versions-1
                   (list (cons "/home/ewen/bin/clojure-1.6.00-alpha45.jar" '("1.6.00" "alpha" "45"))
                         (cons "/home/ewen/bin/clojure-1.6.00.jar" '("1.6.00" nil "0"))
                         (cons "/home/ewen/bin/clojure-1.2-beta.jar" '("1.2" "beta" "0"))
                         (cons "/home/ewen/bin/clojure-1.2.2-SNAPSHOT.jar" '("1.2.2" "SNAPSHOT" "0"))
                         (cons "/home/ewen/bin/clojure-1.6.0.jar" '("1.6.0" nil "0")))))))
