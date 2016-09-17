(ns ewen.replique.tooling
  (:require [clojure.set]
            [ewen.replique.server :refer [with-tooling-response] :as server]
            [ewen.replique.server-cljs :as server-cljs]
            [ewen.replique.repliquedoc :as repliquedoc]
            [clojure.tools.reader :as reader]
            [cljs.tagged-literals :as tags]
            [compliment.core :as compliment]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [compliment.environment :refer [->CljsCompilerEnv]]
            [compliment.context :as context]))

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

(defmethod server/tooling-msg-handle :repliquedoc-clj
  [{:keys [context ns symbol] :as msg}]
  (with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc nil ns context symbol)}))

(defmethod server/tooling-msg-handle :repliquedoc-cljs
  [{:keys [context ns symbol] :as msg}]
  (with-tooling-response msg
    {:doc (repliquedoc/handle-repliquedoc
           (->CljsCompilerEnv @server-cljs/compiler-env)
           ns context symbol)}))

(comment

  (context/cache-context "[(print __prefix__)]")

  (context/cache-context "(.ff rr __prefix__)")

  (bindings-from-context
   (context/cache-context "(let [e nil]
__prefix__)"))
  
  )

