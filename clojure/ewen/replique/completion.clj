(ns ewen.replique.completion)

(defn var-symbol?
  "Test if prefix resembles a var name."
  [x]
  (re-matches #"[^\.\/\:]*([^\/\:]+\/[^\.\/\:]*)?" x))

(defn fuzzy-matches?
  "Tests if symbol matches the prefix when symbol is split into parts on
  separator."
  [prefix, ^String symbol, separator]
  (when (or (.startsWith symbol prefix) (= (first prefix) (first symbol)))
    (loop [pre (rest prefix), sym (rest symbol), skipping false]
      (cond (empty? pre) true
            (empty? sym) false
            skipping (if (= (first sym) separator)
                       (recur (if (= (first pre) separator)
                                (rest pre) pre)
                              (rest sym) false)
                       (recur pre (rest sym) true))
            (= (first pre) (first sym)) (recur (rest pre) (rest sym) false)
            :else (recur pre (rest sym) (not= (first sym) separator))))))

(defn dash-matches?
  "Tests if prefix partially matches a var name with dashes as
  separators."
  [prefix var]
  (fuzzy-matches? prefix var \-))

(defn resolve-namespace
  "Tries to resolve a namespace from the given symbol, either from a
  fully qualified name or an alias in the given namespace."
  [sym ns cljs-comp-env]
  (or (ewen.replique.reflection/find-ns sym cljs-comp-env)
      (get (ewen.replique.reflection/ns-aliases ns cljs-comp-env)
           sym)))

(defn get-scope-and-prefix
  "Tries to get take apart scope namespace and prefix in prefixes like
  `scope/var`."
  [^String s ns cljs-comp-env]
  (let [[scope-name sym] (if (> (.indexOf s "/") -1)
                           (.split s "/") ())
        scope (when scope-name
                (resolve-namespace
                 (symbol scope-name)
                 ns
                 cljs-comp-env))
        prefix (if scope
                 (or sym "") s)]
    [scope-name scope prefix]))

(defn candidates-ns-mappings
  "Returns a list of namespace-bound candidates, with namespace being
  either the scope (if prefix is scoped), `ns` arg or the namespace
  extracted from context if inside `ns` declaration."
  [^String prefix ns context cljs-comp-env]
  (if (var-symbol? prefix)
    (let [[scope-name scope ^String prefix]
          (get-scope-and-prefix prefix ns cljs-comp-env)
          ns-form-namespace nil #_(try-get-ns-from-context context)
          vars (cond
                 scope (ewen.replique.reflection/ns-publics
                        scope cljs-comp-env)
                 ns-form-namespace
                 (ewen.replique.reflection/ns-publics
                  ns-form-namespace cljs-comp-env)
                 :else (ewen.replique.reflection/ns-map
                        ns cljs-comp-env))]
      (for [[var _] vars
            :let [var-name (name var)]
            :when (dash-matches? prefix var-name)]
        (if scope
          (str scope-name "/" var-name)
          var-name)))))











(defn nscl-symbol?
  "Tests if prefix looks like a namespace or classname."
  [x]
  (re-matches #"[^\/\:\.][^\/\:]+" x))

(defn nscl-matches?
  "Tests if prefix partially matches a var name with periods as
  separators."
  [prefix namespace]
  (fuzzy-matches? prefix namespace \.))

(defn candidates-ns
  "Returns a list of namespace completions"
  [^String prefix ns context cljs-comp-env]
  (when (nscl-symbol? prefix)
    (let [has-dot (> (.indexOf prefix ".") -1)]
      ((comp distinct concat)
       (for [ns-str (concat (map (comp name ns-name)
                                 (ewen.replique.reflection/all-ns
                                  cljs-comp-env))
                            (when-not has-dot
                              (->> (ewen.replique.reflection/ns-aliases
                                    ns
                                    cljs-comp-env)
                                   keys
                                   (map name))))
             :when (nscl-matches? prefix ns-str)]
         ns-str)
       (if has-dot
         (for [ns-str (ewen.replique.classpath/namespaces-on-classpath
                       (if cljs-comp-env
                         (.. Thread currentThread
                             getContextClassLoader)
                         (.. clojure.lang.RT baseLoader)))
               :when (nscl-matches? prefix ns-str)]
           ns-str)
         (for [^String ns-str
               (ewen.replique.classpath/namespaces-on-classpath
                (if cljs-comp-env
                  (.. Thread currentThread
                      getContextClassLoader)
                  (.. clojure.lang.RT baseLoader)))
               :when (.startsWith ns-str prefix)]
           ns-str))))))

(defn user-ns [cljs-comp-env]
  (if-not cljs-comp-env
    'user
    'cljs.user))

(when (not (find-ns 'cljs.analyzer))
  (create-ns 'cljs.analyzer)
  (intern 'cljs.analyzer '*cljs-ns* 'cljs.user))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [ns cljs-comp-env]
  (cond (instance? clojure.lang.Namespace ns) ns
        (instance? ewen.replique.reflection.CljsNamespace ns) ns
        (symbol? ns) (or (ewen.replique.reflection/find-ns
                          ns cljs-comp-env)
                         (ewen.replique.reflection/find-ns
                          (user-ns cljs-comp-env)
                          cljs-comp-env))
        :else (if-not cljs-comp-env
                *ns*
                (ewen.replique.reflection/find-ns
                 cljs.analyzer/*cljs-ns*
                 cljs-comp-env))))

(defn completions
  ([prefix]
   (completions prefix {}))
  ([prefix {:keys [ns cljs-comp-env] :as options-map}]
   (let [ns (ensure-ns ns cljs-comp-env)]
     (into (candidates-ns-mappings prefix ns nil cljs-comp-env)
           (candidates-ns prefix ns nil cljs-comp-env)))))


(comment
  (get (:cljs.analyzer/namespaces @compiler-env) 'ewen.replique.test3)

  {:name ewen.replique.test3, :doc nil, :excludes #{}, :use-macros nil, :require-macros nil, :uses {pr cljs.core, prn cljs.core}, :requires {cc cljs.core, cljs.core cljs.core}, :imports nil, :defs {rr {:name ewen.replique.test3/rr, :file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :line 4, :column 1, :end-line 4, :end-column 8, :meta {:file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :line 4, :column 6, :end-line 4, :end-column 8}}, tt {:protocol-inline nil, :meta {:file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :line 6, :column 8, :end-line 6, :end-column 10, :private true, :arglists (quote ([]))}, :private true, :name ewen.replique.test3/tt, :variadic false, :file "/home/egr/replique.el/clojure/ewen/replique/out/ewen/replique/test3.cljs", :end-column 10, :method-params ([]), :protocol-impl nil, :arglists-meta (nil nil), :column 1, :line 6, :end-line 6, :max-fixed-arity 0, :fn-var true, :arglists (quote ([]))}}}

  (ewen.replique.reflection/find-ns 'ewen.replique.test3 @compiler-env)
  (ewen.replique.reflection/find-ns 'blabla @compiler-env)
  (ewen.replique.reflection/ns-publics 'ewen.replique.test3 @compiler-env)
  (count (ewen.replique.reflection/ns-core-refers 'ewen.replique.test3 @compiler-env))
  (first (ewen.replique.reflection/ns-core-refers 'ewen.replique.test3 @compiler-env))
  (count (ewen.replique.reflection/ns-map 'ewen.replique.test3 @compiler-env))
  )
