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
            [compliment.environment :refer [->CljsCompilerEnv]]))

(defmethod server/tooling-msg-handle :clj-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    {:candidates (compliment/completions prefix {:ns ns :context context})}))


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

(defmethod server/tooling-msg-handle :cljs-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (binding [reader/*data-readers*
                                      tags/*cljs-data-readers*]
                              (reader/read-string context)))]
      {:candidates (compliment/completions
                    prefix
                    {:ns ns :context ctx
                     :comp-env (->CljsCompilerEnv @server-cljs/compiler-env)
                     :sources
                     [:compliment.sources.ns-mappings/ns-mappings
                      :compliment.sources.namespaces-and-classes/namespaces-and-classes]})})))


(comment
  (count
   (:candidates (server/tooling-msg-handle {:type :cljs-completion
                                            :context nil
                                            :ns 'cljs.user
                                            :prefix "goog"})))
  )
