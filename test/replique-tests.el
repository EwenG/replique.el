;;; -*- lexical-binding: t; -*-

;;; Code:

(require 'replique)

(setq-local clojure-jars-1 "/home/ewen/bin/clojure-1.6.00-alpha45.jar /home/ewen/bin/clojure-1.6.00.jar /home/ewen/bin/clojure-1.2-beta.jar /home/ewen/bin/clojure-1.2.2-SNAPSHOT.jar /home/ewen/bin/clojure-1.6.0.jar")



(ert-deftest test-replique-clojure-jar->version ()
  (setq-local jar-1 "/home/ewen/bin/clojure-1.6.00-alpha45.jar")
  (should (equal (replique-clojure-jar->version jar-1)
                 (cons jar-1 '("1.6.00" "alpha" "45")))))
