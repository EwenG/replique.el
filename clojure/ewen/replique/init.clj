(ns ewen.replique.init)

(def ^:const init-files
  ["clojure/ewen/replique/classpath.clj"
   "clojure/ewen/replique/reflection.clj"
   "clojure/ewen/replique/completion.clj"
   "clojure/ewen/replique/lein.clj"
   "clojure/ewen/replique/core.clj"])

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
