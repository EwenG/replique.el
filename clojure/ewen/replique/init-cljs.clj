(ns ewen.replique.init
  (require [cljs.repl]
           [cljs.repl.browser]))

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
  (cljs.repl/repl (cljs.repl.browser/repl-env)
                  :output-dir "out"))
