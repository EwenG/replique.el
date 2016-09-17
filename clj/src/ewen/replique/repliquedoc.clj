(ns ewen.replique.repliquedoc
  (:refer-clojure :exclude [find-ns meta])
  (:require [clojure.set]
            [compliment.core :as compliment]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [compliment.environment :refer [->CljsCompilerEnv find-ns meta]]
            [compliment.context :as context]
            [compliment.utils :refer [resolve-class]]
            [compliment.sources.class-members :refer [class-member-symbol?
                                                      static-member-symbol?
                                                      try-get-object-class
                                                      get-all-members
                                                      static-members]]
            [compliment.sources.local-bindings :refer [bindings-from-context]])
  (:import [compliment.environment CljsCompilerEnv]
           [java.lang.reflect Method Member Modifier]))

(defmulti format-call (fn [{:keys [type]}] type))

(defmethod format-call :local-binding [{:keys [sym]}]
  (format "%s: local binding" (name sym)))

(defmethod format-call :var [{:keys [var]}]
  (let [{:keys [ns name arglists]} (meta var)]
    (cond (and name arglists)
          (format "%s/%s: %s" ns name (print-str arglists))
          name
          (format "%s/%s" ns name)
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
  (server/tooling-msg-handle {:type :repliquedoc
                              :context "(.code __prefix__)
"
                              :ns "ewen.replique.tooling"
                              :symbol "ee3"})
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
  (format "%s: %s" (name sym) (print-str arglists)))

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
    (cond (get bindings (name fn-sym))
          {:type :local-binding :sym fn-sym}
          (get special-forms-clj fn-sym)
          {:type :special-form :sym fn-sym :arglists (get special-forms-arglists fn-sym)}
          :else {:type :var :var (ns-resolve ns fn-sym)})))

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
    (when (class-member-symbol? (name method-sym))
      (let [;; Remove the starting "."
            method-name (subs (name method-sym) 1)
            object (if (= '__prefix__ object)
                     sym-at-point
                     object)
            object (when (and (symbol? object)
                              (not (get bindings (name object))))
                     (ns-resolve ns object))
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
  (server/tooling-msg-handle {:type :repliquedoc
                              :context "(__prefix__)
"
                              :ns "ewen.replique.tooling"
                              :symbol "Integer/compare"})
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
