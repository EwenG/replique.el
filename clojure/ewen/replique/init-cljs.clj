(ns ewen.replique.init
  (:require [cljs.repl]
           [cljs.repl.browser]
           [cljs.env :as cljs-env]
           [cljs.compiler :as comp]
           [cljs.analyzer :as ana]
           [cljs.closure :as closure]
           [clojure.java.io :as io]
           [cljs.js-deps :as deps]
           [cljs.util :as util])
  (:import [java.io File]))

(when (not (find-ns 'ewen.replique.core))
  (create-ns 'ewen.replique.core)
  (intern 'ewen.replique.core 'tooling-msg-handle nil))
(when (not (find-ns 'ewen.replique.completion))
  (create-ns 'ewen.replique.completion)
  (intern 'ewen.replique.completion 'completions nil))
(when (not (find-ns 'ewen.replique.classpath))
  (create-ns 'ewen.replique.classpath)
  (intern 'ewen.replique.classpath 'classloader-hierarchy nil))

(def compiler-env (atom nil))
(def repl-env (atom nil))
(defonce env {:context :expr :locals {}})

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
                   :platform "cljs"
                   :load-file-fn
                   #(cljs-env/with-compiler-env @compiler-env
                      (cljs.repl/load-file repl-env % opts))
                   :in-ns-fn
                   (make-in-ns-fn repl-env env)
                   :completion-fn
                   (fn [prefix options-map]
                     (ewen.replique.completion/completions
                      prefix (assoc options-map
                                    :cljs-comp-env
                                    @cljs-env/*compiler*)))
                   :classloader-hierarchy-fn
                   (partial
                    ewen.replique.classpath/classloader-hierarchy
                    (.. Thread currentThread
                        getContextClassLoader)))))))})


(alter-var-root #'cljs.repl/default-special-fns
                  #(merge % special-fns))

(alter-var-root #'cljs.repl.browser/compile-client-js
                (constantly
                 (fn [opts]
                   (let [copts {:optimizations :simple
                                :output-dir (:working-dir opts)}]
                     ;; we're inside the REPL process where
                     ;; cljs.env/*compiler* is already
                     ;; established, need to construct a new one to avoid
                     ;; mutating the one the REPL uses
                     (closure/build
                      '[(ns clojure.browser.repl.client
                          (:require [goog.events :as event]
                                    [clojure.browser.repl :as repl]))
                        (defn start [url]
                          (event/listen js/window
                                        "load"
                                        (fn []
                                          (repl/start-evaluator url))))]
                      copts (cljs-env/default-compiler-env copts))))))

(def ^:const init-files
  ["clojure/ewen/replique/reflection.clj"
   "clojure/ewen/replique/completion.clj"
   "clojure/ewen/replique/classpath.clj"
   "clojure/ewen/replique/lein.clj"
   "clojure/ewen/replique/core.clj"])

(def ^:const env-browser-path "clojure/ewen/replique/cljs_env/browser.cljs")

(when (not (find-ns 'ewen.replique.classpath))
  (create-ns 'ewen.replique.classpath)
  (intern 'ewen.replique.classpath 'classloader-hierarchy nil)
  (intern 'ewen.replique.classpath 'choose-classloader nil))

;;/home/egr/replique.el/
(defn init [replique-root-dir comp-opts repl-opts]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread)
                            (clojure.lang.DynamicClassLoader. cl)))

  (let [opts (-> (read-string comp-opts)
                 (assoc :optimizations :none))
        repl-opts (-> (read-string repl-opts)
                      (assoc :optimizations :none
                             :serve-static true))
        in-env-browser (io/file replique-root-dir env-browser-path)
        env-browser-ns-infos (ana/parse-ns in-env-browser)
        out-env-browser-js (util/to-target-file
                            (:output-dir opts)
                            env-browser-ns-infos)
        out-env-browser-cljs (util/to-target-file
                              (:output-dir opts)
                              env-browser-ns-infos
                              "cljs")]
    (reset! compiler-env (cljs-env/default-compiler-env opts))
    (reset! repl-env (apply cljs.repl.browser/repl-env
                            (apply concat repl-opts)))
    (util/mkdirs out-env-browser-cljs)
    (.createNewFile out-env-browser-cljs)
    (io/copy in-env-browser out-env-browser-cljs)

    (cljs-env/with-compiler-env @compiler-env
      (as-> (slurp in-env-browser) $
        (str "[" $ "]")
        (read-string $)
        (closure/-compile $ opts)
        (closure/output-one-file
         {:output-to (.getAbsolutePath out-env-browser-js)} $))

      (->> (assoc env-browser-ns-infos
                  :file (.getAbsolutePath out-env-browser-js))
           (closure/add-dependencies opts)
           (apply closure/output-unoptimized opts)))

    (closure/output-one-file {:output-to (str (:output-dir opts)
                                              "/index.html")}
                             (str "<html>
    <body>
        <script type=\"text/javascript\" src=\"main.js\"></script>
    </body>
</html>"))

    (let [repl-requires '[[cljs.repl
                           :refer-macros [source doc
                                          find-doc apropos
                                          dir pst]]
                          [cljs.pprint :refer [pprint]
                           :refer-macros [pp]]]
          init-fn (fn []
                    (cljs.repl/evaluate-form
                     @repl-env
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
       (partial cljs.repl/repl @repl-env)
       (->> (merge
             opts
             {:compiler-env @compiler-env
              :init init-fn})
            (into [])
            flatten)))))
