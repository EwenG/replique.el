(ns ewen.replique.tooling
  (:require [clojure.set]
            [ewen.replique.utils :as utils]
            [ewen.replique.tooling-msg :as tooling-msg]
            [ewen.replique.repliquedoc :as repliquedoc]
            [compliment.core :as compliment]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [compliment.environment :refer [->CljsCompilerEnv]]
            [compliment.context :as context]))

(def ^:private cljs-compiler-env
  (utils/dynaload 'ewen.replique.server-cljs/compiler-env))

(defmethod tooling-msg/tooling-msg-handle :clj-completion
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [context]} (context/cache-context nil (when ns (symbol ns)) context)
          context (reverse context)]
     {:candidates (compliment/completions
                   prefix
                   {:ns (when ns (symbol ns))
                    :context context})})))

(comment
  (tooling-msg/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'ewen.replique.server
                              :prefix "tooli"})

  (tooling-msg/tooling-msg-handle {:type :clj-completion
                              :context nil
                              :ns 'compliment.sources
                              :prefix "all-s"})

  (tooling-msg/tooling-msg-handle {:type :clj-completion
                                   :context nil
                                   :ns 'ewen.foo
                                   :prefix "foo"})

  )

(defmethod tooling-msg/tooling-msg-handle :cljs-completion
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [context]} (context/cache-context
                             (->CljsCompilerEnv @@cljs-compiler-env)
                             (when ns (symbol ns)) context)
          context (reverse context)]
      {:candidates (compliment/completions
                    prefix
                    {:ns (when ns (symbol ns)) :context context
                     :comp-env (->CljsCompilerEnv @@cljs-compiler-env)
                     :sources
                     [:compliment.sources.ns-mappings/ns-mappings
                      :compliment.sources.namespaces-and-classes/namespaces-and-classes
                      :compliment.sources.keywords/keywords
                      :compliment.sources.local-bindings/local-bindings
                      :compliment.sources.special-forms/literals
                      :compliment.sources.special-forms/special-forms]})})))

(defmethod tooling-msg/tooling-msg-handle :cljc-completion
  [{:keys [context ns prefix] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [reader-conditionals context]} (context/cache-context
                                                 (->CljsCompilerEnv @@cljs-compiler-env)
                                                 (when ns (symbol ns)) context)
          context (reverse context)]
      (if (= #{:cljs} reader-conditionals)
        {:candidates (compliment/completions
                      prefix
                      {:ns (when ns (symbol ns)) :context context
                       :comp-env (->CljsCompilerEnv @@cljs-compiler-env)
                       :sources
                       [:compliment.sources.ns-mappings/ns-mappings
                        :compliment.sources.namespaces-and-classes/namespaces-and-classes
                        :compliment.sources.keywords/keywords
                        :compliment.sources.local-bindings/local-bindings
                        :compliment.sources.special-forms/literals
                        :compliment.sources.special-forms/special-forms]})}
        {:candidates (compliment/completions
                      prefix
                      {:ns (when ns (symbol ns))
                       :context context})}))))

(comment
  (tooling-msg/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns "ewen.replique.compliment.ns-mappings-cljs-test"
                              :prefix ":cljs.c"})
  
  (tooling-msg/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns "ewen.replique.compliment.ns-mappings-cljs-test"
                              :prefix "::eee"})
  
  )

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-clj
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc nil ns context symbol)}))

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-cljs
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc
           (->CljsCompilerEnv @@cljs-compiler-env)
           ns context symbol)}))

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-cljc
  [{:keys [context ns symbol] :as msg}]
  (tooling-msg/with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc-cljc
           (->CljsCompilerEnv @@cljs-compiler-env)
           ns context symbol)}))

(comment

  (context/cache-context "[(print __prefix__)]")

  (context/cache-context "(.ff rr __prefix__)")

  (bindings-from-context
   (context/cache-context "(let [e nil]
__prefix__)"))
  
  )

(defmethod tooling-msg/tooling-msg-handle :spec-completion
  [{:keys [context prefix spec] :as msg}]
  (tooling-msg/with-tooling-response msg
    (let [{:keys [context]} (context/cache-context nil nil context)]
     {:candidates nil})))

(defmethod tooling-msg/tooling-msg-handle :repliquedoc-spec
  [{:keys [context ns symbol] :as msg}]
  )


