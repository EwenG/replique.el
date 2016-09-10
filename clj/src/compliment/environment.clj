(ns compliment.environment
  "Unify Clojure platforms (Clojure, Clojurescript, ...) environments"
  (:refer-clojure :exclude [ns-name find-ns ns-publics ns-map ns-aliases all-ns meta])
  (:require [compliment.utils :refer [defmemoized all-files-on-classpath]]
            [clojure.set])
  (:import [java.io File]
           [java.lang.reflect Field]))

(defprotocol ICljsCompilerEnv
  (get-wrapped [compile-env]))
(deftype CljsCompilerEnv [wrapped]
  ICljsCompilerEnv
  (get-wrapped [this] wrapped))

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

(def ^:private cljs-ns-var
  (delay (dynaload 'cljs.analyzer/*cljs-ns*)))

(def ^:private cljs-get-js-index
  (delay (dynaload 'cljs.analyzer.api/get-js-index)))

(defprotocol NamespaceEnv
  (all-ns [comp-env])
  (find-ns [comp-env sym])
  (ns-publics [comp-env ns])
  (ns-map [comp-env ns])
  (ns-aliases [comp-env ns]))

(defprotocol Namespace
  (ns-name [ns]))

(defrecord CljsNamespace [name doc excludes use-macros require-macros uses
                          requires imports defs])

(extend-protocol Namespace
  CljsNamespace
  (ns-name [ns]
    (if (symbol? ns) ns
        (:name ns)))
  clojure.lang.Namespace
  (ns-name [ns]
    (clojure.core/ns-name ns)))

(defn ns-core-refers
  "Returns a list of cljs.core vars visible to the ns."
  [comp-env ns]
  (let [vars (ns-publics comp-env 'cljs.core)
        excludes (:excludes ns)]
    (apply dissoc vars excludes)))

(extend-protocol NamespaceEnv
  CljsCompilerEnv
  (all-ns [comp-env]
    (@cljs-all-ns (get-wrapped comp-env)))
  (find-ns [comp-env sym]
    (when-let [found-ns (@cljs-find-ns (get-wrapped comp-env) sym)]
      (map->CljsNamespace found-ns)))
  (ns-publics [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (->> (merge
            (:defs ns)
            (:macros ns))
           (remove (fn [[k v]] (:private v)))
           (into {}))))
  (ns-map [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (->> (select-keys ns [:imports :uses :defs :use-macros])
           (map second)
           (apply merge)
           (merge (ns-core-refers comp-env ns)))))
  (ns-aliases [comp-env ns]
    (let [ns (if (symbol? ns) (find-ns comp-env ns) ns)]
      (:requires ns)))
  nil
  (all-ns [comp-env]
    (clojure.core/all-ns))
  (find-ns [comp-env sym]
    (clojure.core/find-ns sym))
  (ns-publics [_ ns]
    (clojure.core/ns-publics ns))
  (ns-map [_ ns]
    (clojure.core/ns-map ns))
  (ns-aliases [_ ns]
    (clojure.core/ns-aliases ns)))

(defn meta [var]
  (if (= (type var) clojure.lang.Var)
    (clojure.core/meta var)
    (:meta var)))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [comp-env sym ns]
  (or (find-ns comp-env sym) (get (ns-aliases comp-env ns) sym)))

(defprotocol EnvDefaults
  (ns-var [comp-env])
  (file-extension [comp-env])
  (default-ns [comp-env]))

(extend-protocol EnvDefaults
  CljsCompilerEnv
  (ns-var [comp-env] (find-ns comp-env @cljs-ns-var))
  (file-extension [_] "cljs")
  (default-ns [_] 'cljs.user)
  nil
  (ns-var [_] *ns*)
  (file-extension [_] "clj")
  (default-ns [_] 'user))

(defmemoized namespaces-on-classpath
  "Returns the list of all Clojure/Clojurescript/... namespaces obtained by classpath scanning."
  [comp-env]
  (set (for [^String file (all-files-on-classpath)
             :when (and (.endsWith file (file-extension comp-env))
                        (not (.startsWith file "META-INF")))
             :let [[_ ^String nsname] (->
                                       "[^\\w]?(.+)(\\.%s|\\.cljc)"
                                       (format (file-extension comp-env))
                                       re-pattern 
                                       (re-matches file))]
             :when nsname]
         (.. nsname (replace File/separator ".") (replace "_" "-")))))

(defmemoized provides-from-js-dependency-index [comp-env]
  (->> comp-env get-wrapped (@cljs-get-js-index) vals (mapcat :provides) set))

(defprotocol Env
  (keywords [comp-env]))

(defn constant-table->keyword [constant-table]
  (->> (keys constant-table)
       (filter keyword?)
       (map #(symbol (namespace %) (name %)))))

(extend-protocol Env
  CljsCompilerEnv
  (keywords [comp-env]
    (let [global-constant-table (:cljs.analyzer/constant-table @(get-wrapped comp-env))
          constant-tables (->>
                           @(get-wrapped comp-env)
                           :cljs.analyzer/namespaces
                           (map (comp :cljs.analyzer/constant-table second))
                           (cons global-constant-table))]
      (->> (map constant-table->keyword constant-tables)
           (reduce #(into %1 %2) #{}))))
  nil
  (keywords [_]
    (let [^Field field (.getDeclaredField clojure.lang.Keyword "table")]
      (.setAccessible field true)
      (.keySet (.get field nil)))))

(comment
  (require '[ewen.replique.server-cljs :refer [compiler-env]])
  (def comp-env (->CljsCompilerEnv @compiler-env))

  (ns-name (find-ns comp-env 'cljs.user))
  
  (file-extension comp-env)
  (namespaces-on-classpath comp-env)
  (filter #(.endsWith % "cljs") (all-files-on-classpath))
  
  (count (provides-from-js-dependency-index comp-env))

  (:cljs.analyzer/constant-table @@compiler-env)
  (:cljs.analyzer/constants (get (:cljs.analyzer/namespaces @@compiler-env) 'ewen.replique.compliment.ns-mappings-cljs-test))

  (keywords nil)
  (keywords comp-env)

  (ns-name nil (find-ns nil 'ewen.replique.compliment.ns-mappings-clj-test))
  )
