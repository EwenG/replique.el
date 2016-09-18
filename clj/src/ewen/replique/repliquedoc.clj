(ns ewen.replique.repliquedoc
  (:refer-clojure :exclude [find-ns meta ns-resolve ns-name])
  (:require [clojure.set]
            [compliment.core :as compliment]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [compliment.environment :refer [->CljsCompilerEnv find-ns meta
                                            looks-like-var? ns-resolve ns-name]]
            [compliment.context :as context]
            [compliment.utils :refer [resolve-class]]
            [compliment.sources.class-members :refer [class-member-symbol?
                                                      static-member-symbol?
                                                      try-get-object-class
                                                      get-all-members
                                                      static-members]]
            [compliment.sources.local-bindings :refer [bindings-from-context]]
            [compliment.sources.ns-mappings :refer [var-symbol?]])
  (:import [compliment.environment CljsCompilerEnv]
           [java.lang.reflect Method Member Modifier]))

(defmulti format-call (fn [{:keys [type]}] type))

(defmethod format-call :var [{:keys [var comp-env]}]
  (let [{:keys [ns name arglists]} (meta comp-env var)]
    (cond (and name arglists)
          (format "%s/%s: %s" (ns-name ns) name (print-str arglists))
          name
          (format "%s/%s" (ns-name ns) name)
          :else "")))

(defmethod format-call :method [{:keys [method]}]
  (let [klass (.getName (.getDeclaringClass method))
        modifiers (Modifier/toString (.getModifiers method))
        parameter-types (->> (.getParameterTypes method)
                             (map #(.getName %))
                             (clojure.string/join " "))
        return-type (.getName (.getReturnType method))]
    (format "%s %s.%s (%s) -> %s"
            modifiers klass (.getName method)
            parameter-types return-type)))

(defmethod format-call :static-method [{:keys [method]}]
  (let [klass (.getName (.getDeclaringClass method))
        modifiers (Modifier/toString (.getModifiers method))
        parameter-types (->> (.getParameterTypes method)
                             (map #(.getName %))
                             (clojure.string/join " "))
        return-type (.getName (.getReturnType method))]
    (format "%s %s/%s (%s) -> %s"
            modifiers klass (.getName method)
            parameter-types return-type)))

(comment
  (ewen.replique.server/tooling-msg-handle {:type :repliquedoc-clj
                                            :context "(__prefix__)
"
                                            :ns "ewen.replique.repliquedoc"
                                            :symbol "."})

  (ewen.replique.server/tooling-msg-handle {:type :repliquedoc-clj
                                            :context "(clojure.core __prefix__)
"
                                            :ns "ewen.replique.repliquedoc"
                                            :symbol "ee"})
  )
(comment
  (def ee1 Class)
  (def ee2 "e")
  (.getName ee1)
  (.codePointAt ee2)

  (defn ee ([^Double rr & {:keys [ee rr] :or {ee "e" rr "t"}}])
  ([]))
  (ee )
  )


(defmethod format-call :special-form [{:keys [sym arglists]}]
  (format "%s: %s" (str sym) (print-str arglists)))

(def special-forms #{'def 'if 'do 'quote 'recur 'throw 'try 'catch 'new 'set!})
(def special-forms-clj (clojure.set/union
                        special-forms
                        #{'var 'monitor-enter 'monitor-exit}))

(def special-forms-arglists
  {'def '(def symbol doc-string? init?)
   'if '(test then else)
   'do '(do exprs*)
   'quote '(quote form)
   'recur '(recur exprs*)
   'throw '(throw expr)
   'try '(try expr* catch-clause* finally-clause?)
   'catch '(catch classname name expr*)
   'new '[(Classname. args*) (new Classname args*)]
   'set! '[(set! var-symbol expr)
           (set! (. instance-expr instanceFieldName-symbol) expr)
           (set! (. Classname-symbol staticFieldName-symbol) expr)]
   'var '(var symbol)
   'monitor-enter '(monitor-enter x)
   'monitor-exit '(monitor-exit x)})

(defn function-call [compiler-env ns [fn-sym & _] bindings sym-at-point]
  (let [fn-sym (if (= '__prefix__ fn-sym)
                 sym-at-point
                 fn-sym)]
    ;; ns-resolve on something that is not a var may fail
    (when (var-symbol? (str fn-sym))
      (cond (get bindings (str fn-sym))
            nil
            ;; TODO cljs special-forms
            (get special-forms-clj fn-sym)
            {:type :special-form :sym fn-sym :arglists (get special-forms-arglists fn-sym)}
            :else (let [resolved (ns-resolve compiler-env ns fn-sym)]
                    (when (looks-like-var? compiler-env resolved)
                      {:type :var :var resolved :comp-env compiler-env}))))))

(comment
  (let [ee ee]
    (ee "e" 3))
  )

(defn method-with-class? [klass member]
  (when (and (instance? Method member)
         (or (not klass) (= klass (.getDeclaringClass ^Member member))))
    member))

(defn method-call [ns [method-sym object & _] bindings sym-at-point]
  (let [method-sym (if (= '__prefix__ method-sym)
                     sym-at-point
                     method-sym)]
    (when (class-member-symbol? (str method-sym))
      (let [;; Remove the starting "."
            method-name (subs (str method-sym) 1)
            object (if (= '__prefix__ object)
                     sym-at-point
                     object)
            object (when (and (symbol? object)
                              (var-symbol? (str object))
                              (not (get bindings (str object))))
                     (clojure.core/ns-resolve ns object))
            klass (when (= (type object) clojure.lang.Var)
                    (type (deref object)))
            members (get (get-all-members ns) method-name)
            method (some (partial method-with-class? klass) members)]
        (when method
          {:type :method :method method})))))

(defn static-method-call [ns [method-sym object & _] bindings sym-at-point]
  (let [method-sym (if (= '__prefix__ method-sym)
                     sym-at-point
                     method-sym)]
    (when (static-member-symbol? (str method-sym))
      (let [;; Remove the starting "."
            [cl-name method-name] (.split (str method-sym) "/")
            cl (resolve-class ns (symbol cl-name))]
        (when cl
          (let [members (get (static-members cl) method-name)]
            (when (instance? Method (first members))
              {:type :static-method :method (first members)})))))))

(comment
  (server/tooling-msg-handle {:type :repliquedoc-clj
                              :context "(__prefix__)
"
                              :ns "ewen.replique.tooling"
                              :symbol "Integer/compare"})

  (ewen.replique.server/tooling-msg-handle
   {:type :repliquedoc-cljs
    :context "(__prefix__)
"
    :ns "ewen.replique.compliment.ns-mappings-cljs-test"
    :symbol "my-macro"})
  )

(defprotocol Repliquedoc
  (handle-repliquedoc [comp-env ns context sym-at-point]))

(extend-protocol Repliquedoc
  CljsCompilerEnv
  (handle-repliquedoc [comp-env ns context sym-at-point]
    (let [ns (compliment/ensure-ns comp-env (when ns (symbol ns)))
          [{:keys [form]} & _ :as context] (context/cache-context context)
          sym-at-point (and sym-at-point (symbol sym-at-point))]
      (when (and (list? form) (first form) (symbol? (first form)))
        (let [bindings (set (bindings-from-context context))
              fn (delay (function-call comp-env ns form bindings sym-at-point))]
          (when @fn (format-call @fn))))))
  nil
  (handle-repliquedoc [comp-env ns context sym-at-point]
    (let [ns (compliment/ensure-ns comp-env (when ns (symbol ns)))
          [{:keys [form]} & _ :as context] (context/cache-context context)
          sym-at-point (and sym-at-point (symbol sym-at-point))]
      (when (and (list? form) (first form) (symbol? (first form)))
        (let [bindings (set (bindings-from-context context))
              method (delay (method-call ns form bindings sym-at-point))
              static-method (delay (static-method-call ns form bindings sym-at-point))
              fn (delay (function-call comp-env ns form bindings sym-at-point))]
          (cond
            @method
            (format-call @method)
            @static-method
            (format-call @static-method)
            @fn
            (format-call @fn)
            :else nil))))))
