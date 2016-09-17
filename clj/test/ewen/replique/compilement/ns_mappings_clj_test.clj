(ns ewen.replique.compliment.ns-mappings-clj-test
  (:require [ewen.replique.server :as server :refer [tooling-repl]])
  (:import [java.io File]))

(defn my-fn [fff]
  (let [e nil]
    (e))
  (prn "e")
  nil)

(defmacro my-macro [])

::fffffff
