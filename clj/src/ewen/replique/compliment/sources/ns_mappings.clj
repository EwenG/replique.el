(ns ewen.replique.compliment.sources.ns-mappings
  "Completion for vars and classes in the current namespace."
  (:require [ewen.replique.compliment.sources :refer [defsource]]
            [ewen.replique.compliment.utils :refer [fuzzy-matches? resolve-namespace
                                                    *extra-metadata*]]
            [ewen.replique.namespace :as replique-ns])
  (:import java.io.StringWriter))

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"([^\/\:][^\.\/]*([^\/\:]*\/[^\.\/]*)?)?" x))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [^String s ns cljs-comp-env]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace (symbol scope-name) ns cljs-comp-env))
        prefix (if scope
                 (or sym "") s)]
    [scope-name scope prefix]))

(defn try-get-ns-from-context
  "Tries to extract a namespace name if context is a `ns` definition."
  [context]
  (let [[var-list ns-def use-def top-form] context]
    (when (and (sequential? (:form var-list))
               (= (first (:form top-form)) 'ns)
               (or (and (= (first (:form use-def)) :use)
                        (= (second (:form ns-def)) :only))
                   (and (= (first (:form use-def)) :require)
                        (= (second (:form ns-def)) :refer))))
      (find-ns (first (:form ns-def))))))

(defn candidates
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  ([^String prefix ns context]
   (candidates prefix ns context nil))
  ([^String prefix ns context cljs-comp-env]
   (if (var-symbol? prefix)
     (let [[scope-name scope ^String prefix]
           (get-scope-and-prefix prefix ns cljs-comp-env)
           ns-form-namespace (try-get-ns-from-context context)
           vars (cond
                  scope (replique-ns/ns-publics scope cljs-comp-env)
                  ns-form-namespace
                  (replique-ns/ns-publics ns-form-namespace cljs-comp-env)
                  :else (replique-ns/ns-map ns cljs-comp-env))]
       (for [[var-sym var] vars
             :let [var-name (name var-sym)
                   {:keys [arglists doc] :as var-meta} (meta var)]
             :when (dash-matches? prefix var-name)]
         (if (= (type var) Class)
          {:candidate var-name, :type :class,
           :package (when-let [pkg (.getPackage ^Class var)]
                      ;; Some classes don't have a package
                      (.getName ^Package pkg))}

          (cond-> {:candidate (if scope
                                (str scope-name "/" var-name)
                                var-name)
                   :type (cond (:macro var-meta) :macro
                               arglists :function
                               :else :var)
                   :ns (str (or (:ns var-meta) ns))}
            (and arglists(:arglists *extra-metadata*))
            (assoc :arglists (apply list (map pr-str arglists))))))))))

(defsource ::ns-mappings
  :candidates #'candidates
  :doc (constantly nil))

(comment

  (ewen.replique.reflection/find-ns 'ewen.replique.test3 @compiler-env)
  (ewen.replique.reflection/find-ns 'blabla @compiler-env)
  (ewen.replique.reflection/ns-publics 'ewen.replique.test3 @compiler-env)
  (count (ewen.replique.reflection/ns-core-refers 'ewen.replique.test3 @compiler-env))
  (first (ewen.replique.reflection/ns-core-refers 'ewen.replique.test3 @compiler-env))
  (count (ewen.replique.reflection/ns-map 'ewen.replique.test3 @compiler-env))

  (get (:cljs.analyzer/namespaces @@compiler-env) 'cljs.user)

  {:rename-macros {}, :renames {}, :use-macros {doc cljs.repl, find-doc cljs.repl, dir cljs.repl, pst cljs.repl, pp cljs.pprint, source cljs.repl, apropos cljs.repl}, :excludes #{}, :name cljs.user, :imports nil, :requires {cljs.repl cljs.repl, cljs.pprint cljs.pprint}, :uses {pprint cljs.pprint}, :require-macros {cljs.repl cljs.repl, cljs.pprint cljs.pprint}, :doc nil}

  (replique-ns/ns-publics 'ewen.replique.compliment.ns-mappings-cljs-test @compiler-env)
  (replique-ns/ns-publics 'ewen.replique.compliment.ns-mappings-clj-test nil)

  )
