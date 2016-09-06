(ns ewen.replique.namespace
  (:refer-clojure :exclude [find-ns ns-publics ns-map ns-aliases all-ns meta]))

(defn- dynaload
  [s]
  (let [ns (namespace s)]
    (assert ns)
    (require (symbol ns))
    (let [v (resolve s)]
      (if v
        @v
        (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))

(def ^:private cljs-all-ns
  (delay (dynaload 'cljs.analyzer.api/all-ns)))

(def ^:private cljs-find-ns
  (delay (dynaload 'cljs.analyzer.api/find-ns)))

(def ^:private cljs-ns-publics
  (delay (dynaload 'cljs.analyzer.api/ns-publics)))

(defn all-ns
  ([]
   (all-ns nil))
  ([cljs-comp-env]
   (if-not cljs-comp-env
     (clojure.core/all-ns)
     (@cljs-all-ns cljs-comp-env))))

(defrecord CljsNamespace [name doc excludes use-macros require-macros uses
                          requires imports defs])

(defn find-ns
  ([sym]
   (find-ns sym nil))
  ([sym cljs-comp-env]
   (if-not cljs-comp-env
     (clojure.core/find-ns sym)
     (when-let [found-ns (@cljs-find-ns cljs-comp-env sym)]
       (map->CljsNamespace found-ns)))))

(defprotocol Namespace
  (ns-publics [ns comp-env])
  (ns-map [ns comp-env])
  (ns-aliases [ns comp-env]))

(defn ns-core-refers
  "Returns a list of cljs.core vars visible to the ns."
  [ns cljs-comp-env]
  (let [vars (ns-publics 'cljs.core cljs-comp-env)
        excludes (:excludes ns)]
    (apply dissoc vars excludes)))

(extend-protocol Namespace
  CljsNamespace
  (ns-publics [ns cljs-comp-env]
    (->> (merge
          (:defs ns)
          (:macros ns))
         (remove (fn [[k v]] (:private v)))
         (into {})))
  (ns-map [ns cljs-comp-env]
    (->> (select-keys ns [:imports :uses :defs :use-macros])
         (map second)
         (apply merge)
         (merge (ns-core-refers ns cljs-comp-env))))
  (ns-aliases [ns cljs-comp-env]
    (:requires ns))
  clojure.lang.Symbol
  (ns-publics [ns cljs-comp-env]
    (if-not cljs-comp-env
      (clojure.core/ns-publics ns)
      (@cljs-ns-publics cljs-comp-env ns)))
  (ns-map [ns cljs-comp-env]
    (if-not cljs-comp-env
      (clojure.core/ns-map ns)
      (ns-map (find-ns ns cljs-comp-env)
              cljs-comp-env)))
  (ns-aliases [ns cljs-comp-env]
    (if-not cljs-comp-env
      (clojure.core/ns-aliases ns)
      (ns-aliases (find-ns cljs-comp-env)
                  cljs-comp-env)))
  clojure.lang.Namespace
  (ns-publics [ns cljs-comp-env]
    (clojure.core/ns-publics ns))
  (ns-map [ns cljs-comp-env]
    (clojure.core/ns-map ns))
  (ns-aliases [ns cljs-comp-env]
    (clojure.core/ns-aliases ns)))

(defn meta [var]
  (if (= (type var) clojure.lang.Var)
    (clojure.core/meta var)
    (:meta var)))

