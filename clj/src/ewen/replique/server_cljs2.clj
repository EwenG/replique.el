(ns ewen.replique.server-cljs
  (:require [ewen.replique.server :refer [with-tooling-response]
             :as server]
            [clojure.core.server :refer [start-server *session*]]
            [clojure.java.io :as io :refer [file]]
            [cljs.repl.browser]
            [cljs.closure :as closure]
            [cljs.env :as cljs-env]
            [cljs.analyzer :as ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.compiler :as comp]
            [cljs.util :as util]
            [cljs.repl]
            [clojure.string :as string]
            [cljs.js-deps :as deps]
            [clojure.tools.reader :as reader]
            [cljs.closure :as cljsc]
            [ewen.replique.cljs]
            [ewen.replique.sourcemap]
            [ewen.replique.compliment.context :as context]
            [ewen.replique.compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [clojure.data.json :as json]
            [cljs.tagged-literals :as tags]
            [ewen.replique.compliment.core :as compliment])
  (:import [java.io File]
           [java.net URL]
           [java.util.concurrent SynchronousQueue]
           [java.net SocketException]
           [clojure.lang IExceptionInfo]
           [java.util.concurrent.locks ReentrantLock]))

(defonce compiler-env (ref nil))
(defonce repl-env (ref nil))
(defonce eval-queue (SynchronousQueue. true))
(defonce evaled-queue (SynchronousQueue. true))

(defn f->src [f]
  (cond (util/url? f) f
        (.exists (io/file f)) (io/file f)
        :else (io/resource f)))

(defn repl-compile-cljs
  ([f opts]
   (repl-compile-cljs f opts true))
  ([f opts reload-macros]
   (let [src (f->src f)
         compiled (binding [ana/*reload-macros* reload-macros]
                    (closure/compile
                     src
                     (assoc opts
                            :output-file
                            (closure/src-file->target-file src)
                            :force true
                            :mode :interactive)))]
     ;; copy over the original source file if source maps enabled
     (when-let [ns (and (:source-map opts) (first (:provides compiled)))]
       (spit
        (io/file (io/file (util/output-directory opts))
                 (util/ns->relpath ns (util/ext (:source-url compiled))))
        (slurp src)))
     compiled)))

(defn foreign->output-file [foreign opts]
  (let [output-path (closure/rel-output-path
                     (assoc foreign :foreign true)
                     opts)]
    (assoc foreign :file output-path)))

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
        ups-foreign-libs (:ups-foreign-libs opts)
        js-files (deps/find-js-fs (:output-dir opts))
        js-files (map parse-js-fn js-files)
        js-files (filter #(and (seq (:provides %))
                               (not (is-goog %)))
                         js-files)
        js-files (map closure/map->javascript-file js-files)
        js-files (->> ups-foreign-libs
                      (map #(foreign->output-file % opts))
                      (into js-files))]
    (deps/dependency-order js-files)))

(defn repl-cljs-on-disk [compiled repl-opts opts]
  (let [sources (closure/add-dependencies
                 (merge repl-opts opts)
                 compiled)]
    (doseq [source sources]
      (closure/source-on-disk opts source))))

(defmulti init-opts :cljs-env)

(defn init-opts* [{{output-to :output-to} :compiler-env
                   {port :port main :main} :repl-env}]
  (let [output-dir (-> (file output-to) (.getAbsoluteFile) (.getParent))]
    {:compiler-opts (merge {:output-to (-> (file output-to) (.getAbsolutePath))
                            :output-dir output-dir
                            :optimizations :none
                            :recompile-dependents false}
                           (when main {:main main}))
     :repl-opts {:analyze-path []
                 :port port}}))

(defmethod init-opts :browser [{{output-to :output-to} :compiler-env :as opts}]
  (let [opts (init-opts* opts)
        output-dir (get-in opts [:compiler-opts :output-dir])]
    (update-in opts [:repl-opts] merge
               {:serve-static true
                :static-dir ["." output-dir]})))

(defmethod init-opts :webapp [opts]
  (-> (init-opts* opts)
      (update-in [:repl-opts] merge {:serve-static false})))

;; Bypass the cljs load-javascript function in order to use the
;; race condition free "browser-eval" function
(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env provides url]
  (cljs.repl/-evaluate repl-env nil nil (slurp url)))

(defrecord BrowserEnv [wrapped setup-ret]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts] setup-ret)
  (-evaluate [this _ _ js]
    (.put eval-queue js)
    (.take evaled-queue))
  (-load [this provides url]
    (load-javascript this provides url))
  (-tear-down [this] (cljs.repl/-tear-down wrapped))
  cljs.repl/IReplEnvOptions
  (-repl-options [this]
    (cljs.repl/-repl-options wrapped))
  cljs.repl/IParseStacktrace
  (-parse-stacktrace [this st err opts]
    (cljs.repl/-parse-stacktrace wrapped st err opts))
  cljs.repl/IGetError
  (-get-error [this e env opts]
    (cljs.repl/-get-error wrapped e env opts)))

(defn custom-benv [benv setup-ret]
  (merge (BrowserEnv. benv setup-ret) benv))

;; Used instead of cljs.repl.browser to avoid compiling client.js.
;; client.js is not needed because the browser repl uses xhr with cors
;; instead of crosspagechannel
(defn setup [repl-env opts]
  (binding [cljs.repl.browser/browser-state (:browser-state repl-env)
            cljs.repl.browser/ordering (:ordering repl-env)
            cljs.repl.browser/es (:es repl-env)
            cljs.repl.server/state (:server-state repl-env)]
    (println "Waiting for browser to connect ...")
    (cljs.repl.server/start repl-env)))

(defn setup-benv [repl-env]
  (let [setup-ret (setup repl-env nil)]
    (doto (Thread.
           (fn []
             (binding [cljs.repl.browser/browser-state
                       (:browser-state repl-env)
                       cljs.repl.browser/ordering (:ordering repl-env)
                       cljs.repl.browser/es (:es repl-env)
                       cljs.repl.server/state (:server-state repl-env)]
               (loop []
                 (let [js (.take eval-queue)
                       evaled (try
                                (cljs.repl.browser/browser-eval js)
                                (catch SocketException e
                                  {:status :error
                                   :value "Connection broken"}))]
                   (.put evaled-queue evaled))
                 (recur))))
           "repl-env-eval")
      (.setDaemon true)
      (.start))
    setup-ret))

(defn compute-asset-path [asset-path output-dir rel-path]
  (let [asset-path (if asset-path (str "\"" asset-path "\"") "null")
        output-dir (if output-dir (str "\"" output-dir "\"") "null")
        rel-path (if rel-path (str "\"" rel-path "\"") "null")]
    (str "(function(assetPath, outputDir, relPath) {
          if(assetPath) {
            return assetPath;
          }
          var computedAssetPath = assetPath? assetPath : outputDir;
          if(!outputDir ||  !relPath) {
            return computedAssetpath;
          }
          var endsWith = function(str, suffix) {
            return str.indexOf(suffix, str.length - suffix.length) !== -1;
          }
          var origin = window.location.protocol + \"//\" + window.location.hostname + (window.location.port ? ':' + window.location.port: '');
          var scripts = document.getElementsByTagName(\"script\");
          for(var i = 0; i < scripts.length; ++i) {
            var src = scripts[i].src;
            if(src && endsWith(src, relPath)) {
              var relPathIndex = src.indexOf(relPath);
              var originIndex = src.indexOf(origin);
              if(originIndex === 0) {
                return src.substring(origin.length+1, relPathIndex);
              }
            }
          }
          return computedAssetPath;
        })(" asset-path ", " output-dir ", " rel-path ");\n")))

(defn output-main-file [{:keys [closure-defines output-dir output-to]
                         :as opts} port]
  (let [closure-defines (json/write-str closure-defines)
        output-dir-uri (-> output-dir (File.) (.toURI))
        output-to-uri (-> output-to (File.) (.toURI))
        output-dir-path (-> (.normalize output-dir-uri)
                            (.toString))
        output-to-path (-> (.normalize output-to-uri)
                           (.toString))
        ;; If output-dir is not a parent dir of output-to, then
        ;; we don't try to infer the asset path because it may not
        ;; be possible.
        rel-path (if (and (.startsWith output-to-path
                                       output-dir-path)
                          (not= output-dir-path output-to-path))
                   (-> (.relativize output-dir-uri output-to-uri)
                       (.toString))
                   nil)]
    (cljsc/output-one-file
     opts
     (str "(function() {\n"
          "var assetPath = " (compute-asset-path (:asset-path opts) (util/output-directory opts) rel-path)
          "var CLOSURE_UNCOMPILED_DEFINES = " closure-defines ";\n"
          "if(typeof goog == \"undefined\") document.write('<script src=\"'+ assetPath +'/goog/base.js\"></script>');\n"
          "document.write('<script src=\"'+ assetPath +'/cljs_deps.js\"></script>');\n"
          "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"ewen.replique.cljs_env.repl\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
          "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"ewen.replique.cljs_env.browser\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
          (when (:main opts)
            (when-let [main (try (-> (:main opts)
                                     ana-api/parse-ns :ns)
                                 (catch Exception e nil))]
              (str "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge main) "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
                   "document.write('<script>if (typeof goog != \"undefined\") {ewen.replique.cljs_env.repl.connect(\"http://localhost:" port "\");} else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');")))
          "})();\n"))))

(defn init-browser-env
  ([comp-opts repl-opts]
   (init-browser-env comp-opts repl-opts true))
  ([comp-opts repl-opts output-main-file?]
   (let [compiler-env* (-> comp-opts
                           closure/add-implicit-options
                           cljs-env/default-compiler-env)
         repl-env* (apply cljs.repl.browser/repl-env (apply concat repl-opts))]
     (cljs-env/with-compiler-env compiler-env*
       (comp/with-core-cljs nil
         (fn []
           (let [repl-env* (->> (setup-benv repl-env*)
                                (custom-benv repl-env*))]
             (try
               (let [port (-> @(:server-state repl-env*)
                              :socket
                              (.getLocalPort))
                     repl-src "ewen/replique/cljs_env/repl.cljs"
                     benv-src "ewen/replique/cljs_env/browser.cljs"
                     repl-compiled (repl-compile-cljs repl-src comp-opts false)
                     benv-compiled (repl-compile-cljs benv-src comp-opts false)]
                 (repl-cljs-on-disk
                  repl-compiled (#'cljs.repl/env->opts repl-env*) comp-opts)
                 (repl-cljs-on-disk
                  benv-compiled (#'cljs.repl/env->opts repl-env*) comp-opts)
                 (->> (refresh-cljs-deps comp-opts)
                      (closure/output-deps-file
                       (assoc comp-opts :output-to
                              (str (util/output-directory comp-opts)
                                   File/separator "cljs_deps.js"))))
                 (doto (io/file (util/output-directory comp-opts) "goog" "deps.js")
                   util/mkdirs
                   (spit (slurp (io/resource "goog/deps.js"))))
                 (when output-main-file?
                   (output-main-file comp-opts port))
                 (dosync
                  (ref-set compiler-env compiler-env*)
                  (ref-set repl-env repl-env*)))
               (catch Throwable t
                 (cljs.repl/-tear-down repl-env*)
                 (throw t))))))))))

(defmethod server/tooling-msg-handle :set-cljs-env [msg]
  (with-tooling-response msg
    (let [{:keys [compiler-opts repl-opts]} (init-opts msg)]
      (init-browser-env compiler-opts repl-opts)
      {})))

(comment
  (server/tooling-msg-handle
   {:type :set-cljs-env
    :cljs-env :browser
    :compiler-env {:output-to "out"}
    :repl-env {:port 9000}})
  )
