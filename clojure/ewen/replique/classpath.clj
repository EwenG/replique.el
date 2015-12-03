(ns ewen.replique.classpath
  (:refer-clojure :exclude [add-classpath])
  (:require [clojure.java.io :as io])
  (:import [java.io File]
           [java.util.jar JarEntry]
           [java.util.jar JarFile]
           [java.net URL]))


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
  (-> (filter modifiable-classloader? classloaders) last))








(defn list-files
  "Given a path (either a jar file or directory) returns all files under that path."
  [^String path]
  (cond (.endsWith path ".jar")
        (try (for [^JarEntry entry (-> (JarFile. path)
                                       (.entries)
                                       enumeration-seq)
                   :when (not (.isDirectory entry))]
               (.getName entry))
             (catch Exception e))

        (= path "") ()

        :else
        (for [^File file (file-seq (File. path))
              :when (not (.isDirectory file))]
          (.replace ^String (.getPath file) path ""))))

(defn all-files-on-classpath
  "Returns a list of all files on the classpath, including those located inside
  jar files."
  [tip]
  (mapcat #(-> (URL. %)
               (.getPath)
               list-files)
          (-> (classloader-hierarchy tip)
              get-classpath)))

(defn namespaces-on-classpath
  "Returns the list of all Clojure namespaces obtained by classpath
scanning."
  [tip]
  (set (for [^String file (all-files-on-classpath tip)
             :when (and (.endsWith file ".clj")
                        (not (.startsWith file "META-INF")))
             :let [[_ ^String nsname] (re-matches #"[^\w]?(.+)\.clj" file)]
             :when nsname]
         (.. nsname (replace File/separator ".") (replace "_" "-")))))

(comment
  (time (-> (classloader-hierarchy (.. clojure.lang.RT baseLoader))
            get-classpath))

  (list-files
   (.getPath
    (java.net.URL.
     (first
      (->
       (classloader-hierarchy
        (.. clojure.lang.RT baseLoader))
       get-classpath)))))
  (list-files "file:/home/egr/replique.el/lein-project/src" false)


  (list-files (.getPath (URL. "file:/home/egr/replique.el/lein-project/src")))

  (last (all-files-on-classpath (.. clojure.lang.RT baseLoader)))
  (time (namespaces-on-classpath (.. clojure.lang.RT baseLoader)))
  )
