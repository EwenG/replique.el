(ns ewen.replique.core
  (:import [java.lang.reflect Member]
           [java.io File]
           [java.util.jar JarEntry]
           [java.util.jar JarFile]))

(defrecord ToolingMsg [type result])

(def ^:const init-files
  ["clojure/ewen/replique/compliment/sources.clj"
   "clojure/ewen/replique/compliment/utils.clj"
   "clojure/ewen/replique/compliment/sources/ns_mappings.clj"
   "clojure/ewen/replique/compliment/sources/class_members.clj"
   "clojure/ewen/replique/compliment/sources/namespaces_and_classes.clj"
   "clojure/ewen/replique/compliment/sources/keywords.clj"
   "clojure/ewen/replique/compliment/sources/special_forms.clj"
   "clojure/ewen/replique/compliment/sources/local_bindings.clj"
   "clojure/ewen/replique/compliment/sources/resources.clj"
   "clojure/ewen/replique/compliment/context.clj"
   "clojure/ewen/replique/compliment/core.clj"])

(defn init [replique-root-dir]
  (doseq [init-file init-files]
    (load-file (str replique-root-dir init-file)))
  (clojure.main/main))
