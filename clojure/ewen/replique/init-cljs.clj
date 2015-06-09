(ns ewen.replique.init
  (require [cljs.repl]
           [cljs.repl.browser]
           [cljs.env :as cljs-env]
           [cljs.compiler :as comp]
           [cljs.analyzer :as ana]))

(when (not (find-ns 'ewen.replique.core))
  (create-ns 'ewen.replique.core)
  (intern 'ewen.replique.core 'tooling-msg-handle nil))
(when (not (find-ns 'ewen.replique.completion))
  (create-ns 'ewen.replique.completion)
  (intern 'ewen.replique.completion 'completions nil))

(defn make-in-ns-fn [repl-env env]
  (fn [ns-name]
    (let [ns-name (symbol ns-name)]
      (when-not (ana/get-namespace ns-name)
        (swap! cljs-env/*compiler* assoc-in
               [::ana/namespaces ns-name]
               {:name ns-name})
        (cljs.repl/-evaluate
         repl-env "<cljs repl>" 1
         (str "goog.provide('" (comp/munge ns-name) "');")))
      (set! ana/*cljs-ns* ns-name))))

(def special-fns
  {'ewen.replique.core/tooling-msg-handle
   (fn self
     ([repl-env env form]
      (self repl-env env form nil))
     ([repl-env env [_ msg] opts]
      (prn (ewen.replique.core/tooling-msg-handle
              (assoc msg
                     :load-file-fn
                     #(cljs.repl/load-file repl-env % opts)
                     :in-ns-fn
                     (make-in-ns-fn repl-env env)
                     :completion-fn
                     (fn [prefix options-map]
                       (ewen.replique.completion/completions
                        prefix (assoc options-map
                                      :cljs-comp-env
                                      @cljs-env/*compiler*))))))))})

(alter-var-root #'cljs.repl/default-special-fns
                  #(merge % special-fns))

(defonce opts {:output-dir "out"
               :optimizations :none})
(defonce compiler-env (cljs-env/default-compiler-env opts))
(defonce repl-env (cljs.repl.browser/repl-env))
(defonce env {:context :expr :locals {}})

(def ^:const init-files
  ["clojure/ewen/replique/reflection.clj"
   "clojure/ewen/replique/completion.clj"
   "clojure/ewen/replique/core.clj"])

;;/home/egr/replique.el/
(defn init [replique-root-dir]
  (let [repl-requires '[[cljs.repl
                         :refer-macros [source doc
                                        find-doc apropos
                                        dir pst]]
                        [cljs.pprint :refer [pprint]
                         :refer-macros [pp]]]
        init-fn (fn []
                  (cljs.repl/evaluate-form
                   repl-env
                   env
                   "<cljs repl>"
                   (with-meta
                     `(~'ns ~'cljs.user
                        (:require ~@repl-requires))
                     {:line 1 :column 1})
                   identity opts))]
    (doseq [init-file init-files]
      (load-file (str replique-root-dir init-file)))
    (apply
     (partial cljs.repl/repl repl-env)
     (->> (merge
           opts
           {:compiler-env compiler-env
            :init init-fn})
          (into [])
          flatten))))
