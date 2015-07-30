(ns ewen.replique.init)

(def ^:const init-files
  ["clojure/ewen/replique/classpath.clj"
   "clojure/ewen/replique/reflection.clj"
   "clojure/ewen/replique/completion.clj"
   "clojure/ewen/replique/lein.clj"
   "clojure/ewen/replique/core.clj"])

(defmethod print-method clojure.lang.Var [v ^java.io.Writer w]
  (.write w "#object [clojure.lang.Var ")
  (.write w (format "0x%x " (System/identityHashCode v)))
  (print-method (str v) w)
  (.write w "]"))

(defmethod print-method java.util.regex.Pattern [v ^java.io.Writer w]
  (.write w "#object [java.util.regex.Pattern ")
  (.write w (format "0x%x " (System/identityHashCode v)))
  (print-method (str v) w)
  (.write w "]"))

(defn init [replique-root-dir & rest]
  (println "Clojure" (clojure-version))
  (let [init-fn (fn []
                  (apply require
                         '[[clojure.repl :refer [source apropos
                                                 dir pst doc
                                                 find-doc]]
                           [clojure.java.javadoc :refer [javadoc]]
                           [clojure.pprint :refer [pp pprint]]])
                  (doseq [init-file init-files]
                    (load-file (str replique-root-dir init-file))))]
    (clojure.main/repl :init init-fn)))
