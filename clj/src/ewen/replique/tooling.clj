(ns ewen.replique.tooling
  (:require [ewen.replique.server :refer [with-tooling-response] :as server]
            [ewen.replique.server-cljs :as server-cljs]
            [clojure.tools.reader :as reader]
            [cljs.tagged-literals :as tags]
            [ewen.replique.compliment.core :as compliment]
            [ewen.replique.compliment.context :as context]
            [ewen.replique.compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [ewen.replique.compliment.core :as compliment]
            [ewen.replique.compliment.sources :as compliment-sources]))

(defmethod server/tooling-msg-handle :clj-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (read-string context))]
      {:candidates (compliment/completions prefix {:ns ns :context ctx})})))


(comment
  (server/tooling-msg-handle {:type :clj-completion
                       :context nil
                       :ns 'ewen.replique.server
                       :prefix "tooli"})

  (server/tooling-msg-handle {:type :clj-completion
                       :context nil
                       :ns 'ewen.replique.compliment.sources
                       :prefix "all-s"})

  (server/tooling-msg-handle {:type :clj-completion
                       :context nil
                       :ns 'ewen.foo
                       :prefix "foo"})

  (let [eeeeee "e"]
    eee)
  )

(defmethod server/tooling-msg-handle :cljs-completion
  [{:keys [context ns prefix] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (binding [reader/*data-readers*
                                      tags/*cljs-data-readers*]
                              (reader/read-string context)))]
      {:candidates (compliment/completions
                    prefix {:ns ns :context ctx
                            :cljs-comp-env @server-cljs/compiler-env
                            :sources
                            [:ewen.replique.compliment.sources.ns-mappings/ns-mappings]})})))


(comment
  (server/tooling-msg-handle {:type :cljs-completion
                              :context nil
                              :ns 'ewen.replique.cljs-env.repl
                              :prefix "send-r"})
  )
