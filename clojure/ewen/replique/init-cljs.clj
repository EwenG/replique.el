(ns ewen.replique.init
  (:require [cljs.repl]
            [cljs.repl.browser]
            [cljs.env :as cljs-env]
            [cljs.compiler :as comp]
            [cljs.analyzer :as ana]
            [cljs.closure :as closure]
            [clojure.java.io :as io]
            [cljs.js-deps :as deps]
            [cljs.util :as util]
            [clojure.string :as string])
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

(defn refresh-cljs-deps [opts]
  (let [parse-js-fn (fn [js-file]
                      (-> js-file
                          slurp
                          string/split-lines
                          deps/parse-js-ns
                          (assoc :file js-file)))
        is-goog (fn [js-file]
                  (some #(.startsWith % "goog.")
                        (:provides js-file)))
        js-files (deps/find-js-fs (:output-dir opts))
        js-files (map parse-js-fn js-files)
        js-files (filter #(and (seq (:provides %))
                               (not (is-goog %)))
                         js-files)
        js-files (map closure/map->javascript-file js-files)
        js-files (deps/dependency-order js-files)]
    (closure/output-deps-file
     (assoc opts :output-to
            (str (util/output-directory opts)
                 File/separator "cljs_deps.js"))
     js-files)))

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

(defn tooling-msg-handle
  ([repl-env env form]
   (tooling-msg-handle repl-env env form nil))
  ([repl-env env [_ msg] opts]
   ;; The cljs REPL uses "print" to print values evaluated in the
   ;; browser and returned by the browser after a call to "pr-str".
   ;; This is why clojurescript ToolingMsg are wrapped in a "pr-str"
   ;; call.
   (pr-str
    (ewen.replique.core/tooling-msg-handle
     (assoc msg
            :platform "cljs"
            :load-file-fn
            (fn [path]
              (if (.endsWith path "clj")
                (load-file path)
                (cljs-env/with-compiler-env @compiler-env
                  (cljs.repl/load-file repl-env path opts)
                  (try (refresh-cljs-deps opts)
                       (catch AssertionError e (.printStackTrace e))))))
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
                 getContextClassLoader)))))))

(defn eval-cljs [repl-env env form opts]
  (if (and (seq? form)
           (= 'ewen.replique.core/tooling-msg-handle
              (first form)))
    (tooling-msg-handle repl-env env form opts)
    (#'cljs.repl/eval-cljs repl-env env form opts)))

(alter-var-root
 #'cljs.closure/output-main-file
 (constantly
  (fn output-main-file [opts]
    (let [asset-path (or (:asset-path opts)
                         (util/output-directory opts))
          main-requires (->> (for [m (:main opts)]
                               (str "try { goog.require(\""
                                    (comp/munge m)
                                    "\"); } catch(err) {} "))
                             (apply str))
          output-dir-uri (-> (:output-dir opts)
                             (File.)
                             (.toURI))
          output-to-uri (-> (:output-to opts)
                            (File.)
                            (.toURI))
          main-rel-path (.relativize output-dir-uri output-to-uri)]
      (closure/output-one-file
       opts
       (str
        "(function(){"
        "var mainFile = \"" main-rel-path "\";\n"
        "var mainFileParts = mainFile.split(\"/\");\n"
        "var scripts = document.getElementsByTagName(\"script\");\n"
        "var src = scripts[0].src;\n"
        "var srcParts = src.split(\"/\");\n"
        "var assetPath = srcParts.slice(3, srcParts.length-mainFileParts.length).join(\"/\");\n"
        "if(typeof goog == \"undefined\") document.write('<script src=\"' + assetPath + '/goog/base.js\"></script>');\n"
        "document.write('<script src=\"' + assetPath + '/cljs_deps.js\"></script>');\n"
        "document.write('<script>if (typeof goog != \"undefined\") { " main-requires
        " } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
        "})();"))))))








(def ^:const init-files
  ["clojure/ewen/replique/classpath.clj"
   "clojure/ewen/replique/reflection.clj"
   "clojure/ewen/replique/completion.clj"
   "clojure/ewen/replique/lein.clj"
   "clojure/ewen/replique/cljs.clj"
   "clojure/ewen/replique/core.clj"])

(def ^:const env-browser-path "clojure/ewen/replique/cljs_env/browser.cljs")

(when (not (find-ns 'ewen.replique.classpath))
  (create-ns 'ewen.replique.classpath)
  (intern 'ewen.replique.classpath 'classloader-hierarchy nil)
  (intern 'ewen.replique.classpath 'choose-classloader nil))

(defn init-opts* [opts]
  (-> opts
      (assoc-in [:comp-opts :optimizations] :none)
      (update-in [:comp-opts :main]
                 #(-> (into #{} %)
                      (conj 'ewen.replique.cljs-env.browser)))))

(defn init-cljs-env*
  [replique-root-dir
   {:keys [cljs-env-name comp-opts repl-opts]}]
  (let [cl (.getContextClassLoader (Thread/currentThread))]
    (.setContextClassLoader (Thread/currentThread)
                            (clojure.lang.DynamicClassLoader. cl)))
  (let [opts (assoc comp-opts
                    :optimizations :none
                    :main (-> (into #{} (:main comp-opts))
                              (conj 'ewen.replique.cljs-env.browser)))
        repl-opts (assoc repl-opts :optimizations :none)
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
    {:cljs-env-name cljs-env-name
     :comp-opts opts
     :repl-opts repl-opts}))

(defn start-repl [replique-root-dir opts]
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
            :init init-fn
            :eval eval-cljs})
          (apply concat)))))

(defmulti init-cljs-env
  (fn [replique-root-dir
       {:keys [cljs-env-name comp-opts repl-opts]}]
    cljs-env-name))

(defmethod init-cljs-env "browser-env"
  [replique-root-dir init-opts]
  (let [{{output-dir :output-dir} :comp-opts} init-opts
        init-opts (init-opts* init-opts)]
    (init-cljs-env*
     replique-root-dir
     (update-in init-opts [:repl-opts]
                #(assoc %
                        :serve-static true
                        :static-dir output-dir)))
    (closure/output-one-file {:output-to (str output-dir
                                              "/index.html")}
                             (str "<html>
    <body>
        <script type=\"text/javascript\" src=\"main.js\"></script>
    </body>
</html>"))
    (start-repl replique-root-dir (:comp-opts init-opts))))

(defmethod init-cljs-env "webapp-env"
  [replique-root-dir init-opts]
  (let [{{output-dir :output-dir} :comp-opts} init-opts
        init-opts (init-opts* init-opts)]
    (init-cljs-env*
     replique-root-dir
     (assoc-in init-opts [:repl-opts :serve-static] false))
    (start-repl replique-root-dir (:comp-opts init-opts))))

;;
(comment
  "/home/egr/replique.el/"
  (str {:comp-opts {:output-dir "out"
                    :output-to "out/main.js"
                    :recompile-dependents nil
                    :main '(ewen.replique.cljs-env.browser)
                    :asset-path "."}
        :repl-opts {:src nil
                    :static-dir '("out")
                    :port 9000}
        :cljs-env-name "browser-env"})
  )
(defn init [replique-root-dir init-opts]
  (init-cljs-env
   replique-root-dir
   (read-string init-opts)))

















(comment
  (def optss {:output-to "/home/egr/replique.el/webapp-project/resources/public/js/main.js"
              :output-dir "/home/egr/replique.el/webapp-project/resources/public/js/"
              :main '[ewen.test ewen.replique.cljs-env.browser]
              :optimizations :none})
  (import '[java.io File])
  (require '[cljs.analyzer :as ana])
  (require '[cljs.closure :as closure])
  (require '[cljs.js-deps :as deps])
  (require '[cljs.compiler :as cljc])
  (require '[cljs.env :as env])

  (env/with-compiler-env (env/default-compiler-env)
    (refresh-cljs-deps optss))
  )
