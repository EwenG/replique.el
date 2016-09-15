(ns ewen.replique.tooling
  (:require [ewen.replique.server :refer [with-tooling-response] :as server]
            [ewen.replique.server-cljs :as server-cljs]
            [clojure.tools.reader :as reader]
            [cljs.tagged-literals :as tags]
            [compliment.core :as compliment]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [compliment.core :as compliment]
            [compliment.sources :as compliment-sources]
            [compliment.environment :refer [->CljsCompilerEnv]]
            [compliment.context :as context]
            [compliment.sources.class-members :refer [class-member-symbol?
                                                      try-get-object-class
                                                      get-all-members]]
            [compliment.sources.local-bindings :refer [bindings-from-context]])
  (:import [java.lang.reflect Method Member]))

(defmethod server/tooling-msg-handle :clj-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    {:candidates (compliment/completions
                  prefix
                  {:ns (when ns (symbol ns))
                   :context context})}))

(comment
  (server/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'ewen.replique.server
                              :prefix "tooli"})

  (server/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'compliment.sources
                              :prefix "all-s"})

  (server/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'ewen.foo
                              :prefix "foo"})

  )

(defmethod server/tooling-msg-handle :cljs-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    {:candidates (compliment/completions
                  prefix
                  {:ns (when ns (symbol ns)) :context context
                   :comp-env (->CljsCompilerEnv @server-cljs/compiler-env)
                   :sources
                   [:compliment.sources.ns-mappings/ns-mappings
                    :compliment.sources.namespaces-and-classes/namespaces-and-classes
                    :compliment.sources.keywords/keywords
                    :compliment.sources.local-bindings/local-bindings
                    :compliment.sources.special-forms/literals
                    :compliment.sources.special-forms/special-forms]})}))

(comment
  (server/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns "ewen.replique.compliment.ns-mappings-cljs-test"
                              :prefix ":cljs.c"})
  
  (server/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns "ewen.replique.compliment.ns-mappings-cljs-test"
                              :prefix "::eee"})
  
  )

(defn format-function-call [fn-var]
  (let [{:keys [name arglists]} (meta fn-var)]
    (if (and name arglists)
      (format "%s: %s" name (print-str arglists))
      "")))

(defn function-call [ns [fn-sym & _] bindings sym-at-point]
  (let [fn-sym (if (= '__prefix__ fn-sym)
                 sym-at-point
                 fn-sym)]
    (if (get bindings (name fn-sym))
      :local-binding
      (ns-resolve ns fn-sym))))

(comment
  (server/tooling-msg-handle {:type :repliquedoc
                              :context "(let [ee nil]
  (ee __prefix__))
"
                              :ns "ewen.replique.server.tooling"
                              :symbol "d"})

  (server/tooling-msg-handle {:type :repliquedoc
                              :context "(defn __prefix__? [klass member]
  (when (and (instance? Method member)
         (or (not klass) (= klass (.getDeclaringClass ^Member member))))
    member))
"
                              :ns "ewen.replique.server.tooling"
                              :symbol "method-with-class?"})
  )

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
            members (get (get-all-members ns) method-name)]
        (some (partial method-with-class? klass) members)))))

(defn handle-repliquedoc [comp-env ns context sym-at-point]
  (let [ns (compliment/ensure-ns comp-env (when ns (symbol ns)))
        [{:keys [form]} & _ :as context] (context/cache-context context)
        sym-at-point (and sym-at-point (symbol sym-at-point))]
    (when (and (list? form) (first form))
      (let [bindings (set (bindings-from-context context))
            member (delay (method-call ns form bindings sym-at-point))
            fn (delay (function-call ns form bindings sym-at-point))]
        (cond
          @member
          (.getName @member)
          @fn
          (if (= :local-binding @fn)
            "Local binding"
            (format-function-call @fn))
          :else nil)))))

(defmethod server/tooling-msg-handle :repliquedoc
  [{:keys [context ns symbol] :as msg}]
  (with-tooling-response msg
    {:doc (handle-repliquedoc nil ns context symbol)}))

(comment

  (context/cache-context "[(print __prefix__)]")

  (context/cache-context "(.ff rr __prefix__)")

  (bindings-from-context
   (context/cache-context "(let [e nil]
__prefix__)"))
  
  )

