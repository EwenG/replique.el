(ns ewen.replique.classpath
  (:refer-clojure :exclude [add-classpath])
  (:require [clojure.java.io :as io]))

(defn classloader-hierarchy [tip]
  (->> tip
       (iterate #(.getParent %))
       (take-while boolean)))

(defn get-classpath [classloaders]
  (->> (reverse classloaders)
       (mapcat #(.getURLs %))
       (map str)))

(defn modifiable-classloader? [cl]
  (if (or (= (type cl) clojure.lang.DynamicClassLoader)
          (= (type cl) java.net.URLClassLoader))
    true
    false))

(defn add-classpath [cl file-or-dir]
  (let [url (-> (io/file file-or-dir)
                .toURI
                .toURL)]
    (cond (= (type cl) clojure.lang.DynamicClassLoader)
          (do (.addURL cl url)
              file-or-dir)
          (= (type cl) java.net.URLClassLoader)
          (do (-> java.net.URLClassLoader
                  (.getDeclaredMethod
                   "addURL" (into-array Class [java.net.URL]))
                  (doto (.setAccessible true))
                  (.invoke cl (into-array java.net.URL [url])))
              file-or-dir)
          :else nil)))

(defn choose-classloader [classloaders]
  (-> (filter modifiable-classloader? classloaders)
      last))
