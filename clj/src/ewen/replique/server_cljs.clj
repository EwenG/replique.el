(ns ewen.replique.server-cljs
  (:require [ewen.replique.server :refer [with-tooling-response]
             :as server]
            [ewen.replique.elisp-printer :as elisp]
            [clojure.core.server :refer [*session*]]
            [clojure.java.io :as io]
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
            [cljs.closure :as cljsc]
            [ewen.replique.cljs]
            [clojure.data.json :as json]
            [clojure.spec :as s]
            [cljs.repl.server]
            [cljs.stacktrace :as st]
            [clojure.edn :as edn])
  (:import [java.io File BufferedReader InputStreamReader]
           [java.util.concurrent ArrayBlockingQueue Executors ThreadFactory
            RejectedExecutionException ExecutorService TimeUnit]
           [java.net SocketException ServerSocket InetAddress]
           [clojure.lang IExceptionInfo]
           [java.util.concurrent.locks ReentrantLock]))

(defonce compiler-env (atom nil))
(defonce repl-env (atom nil))
(defonce cljs-outs (atom #{}))
(defonce cljs-server (atom {:state :stopped}))
(def ^:dynamic *stopped-executor?* false)
(defonce cljs-core-bindings #{#'*assert* #'*print-length* #'*print-meta* #'*print-level*
                              #'*flush-on-newline* #'*print-readably* #'*print-dup*})

(def default-repl-requires '[[cljs.repl :refer-macros [source doc find-doc apropos dir pst]]
                             [cljs.pprint :refer [pprint] :refer-macros [pp]]])
(def env {:context :expr :locals {}})

(defmacro ^:private with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr
                    {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))




(defn dispatcher [{:keys [method path content]} conn]
  (cond (and (= :get method) (some #(.endsWith path %)
                                   (keys cljs.repl.browser/ext->mime-type)))
        :assets
        (and (= :get method) (= path "/"))
        :init
        (and (= :post method) (= :ready (:type content)))
        :ready
        (and (= :post method)
             (not= :ready (:type content))
             (not= (:session content) (:session @cljs-server)))
        :session-expired
        (and (= :post method) (= :result (:type content)))
        :result
        (and (= :post method) (= :print (:type content)))
        :print))

(defmulti dispatch-request dispatcher)

;; Same as cljs.repl/send-and-close but supports CORS
(defn send-and-close
  ([conn status form]
   (send-and-close conn status form "text/html"))
  ([conn status form content-type]
   (send-and-close conn status form content-type "UTF-8"))
  ([conn status form content-type encoding]
   (let [byte-form (.getBytes form encoding)
         content-length (count byte-form)
         headers (map #(.getBytes (str % "\r\n"))
                      [(#'cljs.repl.server/status-line status)
                       "Server: ClojureScript REPL"
                       (str "Content-Type: "
                            content-type
                            "; charset=" encoding)
                       (str "Content-Length: " content-length)
                       (str "Access-Control-Allow-Origin: *")
                       (str "Access-Control-Allow-Methods: GET,POST")
                       ""])]
     (with-open [os (.getOutputStream conn)]
       (doseq [header headers]
         (.write os header 0 (count header)))
       (.write os byte-form 0 content-length)
       (.flush os)
       (.close conn)))))

(defn close-conn [conn]
  (try (send-and-close conn 500 "ignore__")
       (catch Exception e nil)
       (finally (.close conn))))

(defmethod dispatch-request :session-expired [request conn]
  (close-conn conn))

(defmethod dispatch-request :default [{:keys [path]} conn]
  (cljs.repl.server/send-404 conn path))

(defn- handle-connection [conn]
  (let [rdr (BufferedReader. (InputStreamReader. (.getInputStream conn)))
        {:keys [content] :as request} (cljs.repl.server/read-request rdr)
        request (if content (update request :content read-string) request)]
    (if request
      (dispatch-request request conn)
      (close-conn conn))))

(defn start-cljs-server [host port]
  {:pre [(not (nil? @compiler-env))]}
  (when (= :stopped (:state @cljs-server))
    ;; host=nil returns loopback
    (let [ss (ServerSocket. port 0 (InetAddress/getByName host)) 
          accept-fn (bound-fn []
                      (when-let [conn (try (.accept ss)
                                           (catch Exception _))]
                        (.setKeepAlive conn true)
                        (handle-connection conn)
                        (recur)))
          accept-thread (Thread. accept-fn)]
      (.start accept-thread)
      (reset! cljs-server {:server-socket ss :port port
                           :accept-thread accept-thread
                           :state :starting}))))

(defn shutdown-executor [executor]
  (let [pendingTasks (.shutdownNow executor)]
        ;; Tasks are run on this thread
        (binding [*stopped-executor?* true]
          (doseq [task pendingTasks]
            (.run task)))))

(defn stop-cljs-server []
  (let [{:keys [server-socket executor conn-queue accept-thread]} @cljs-server]
    (swap! cljs-server assoc :state :stopped)
    (when executor
      (shutdown-executor executor))
    (when server-socket
      (.close server-socket))
    (when accept-thread
      (.interrupt accept-thread))
    (when-let [conn (and conn-queue (.poll conn-queue))]
      (close-conn conn))))

(defmethod dispatch-request :init [{{host :host} :headers} conn]
  (let [url (format "http://%s" host)]
    (send-and-close
     conn 200
     (str "<html>
<head></head>
<body>
<script>var CLOSURE_UNCOMPILED_DEFINES = null;</script>
<script src=\"goog/base.js\"></script>
<script src=\"cljs_deps.js\"></script>
<script>
goog.require(\"ewen.replique.cljs_env.repl\");
</script>
<script>
goog.require(\"ewen.replique.cljs_env.browser\");
</script>
<script>
ewen.replique.cljs_env.repl.connect(\"" url "\");
</script>
</body>
</html>")
     "text/html")))

(defmethod dispatch-request :assets [request conn]
  (cljs.repl.browser/send-static request conn (cljs.repl/-repl-options @repl-env)))

(defn make-eval-task [js]
  (reify Callable
    (call [this]
      (if *stopped-executor?*
        {:status :error
         :value "Connection broken"}
        (try
          (let [{:keys [conn-queue result-queue]} @cljs-server
                conn (try (.take conn-queue) (catch InterruptedException e nil))]
            (if-not conn
              {:status :error
               :value "Connection broken"}
              (try
                (send-and-close conn 200 js)
                (.take result-queue)
                ;; If the repl-env is shutdown
                (catch InterruptedException e
                  {:status :error
                   :value "Connection broken"})
                ;; Other errors, socket closed by the client ...
                (catch SocketException e
                  {:status :error
                   :value "Connection broken"})
                (finally
                  (.close conn))))))))))

(defn init-core-bindings []
  `(do
     ~@(for [v cljs-core-bindings]
         `(~'set! ~(symbol "cljs.core" (-> v meta :name str)) ~(deref v)))))

(defmethod dispatch-request :ready [request conn]
  ;; The server accepts connection on a single thread, which remove the need for handling
  ;; synchronization for some of the operations below
  (let [{:keys [executor conn-queue result-queue session]
         :or {session 0}} @cljs-server
        new-executor (Executors/newSingleThreadExecutor)
        conn-queue (ArrayBlockingQueue. 1)
        result-queue (ArrayBlockingQueue. 1)]
    (when executor (shutdown-executor executor))
    (when-let [conn (and conn-queue (.poll conn-queue))]
      (close-conn conn))
    (swap! cljs-server assoc
           :executor new-executor
           :conn-queue conn-queue
           :result-queue result-queue
           :session (inc session))
    ;; Init stuff needs to go there and not in the :init method of the REPL, otherwise it
    ;; get lost on browser refresh
    (->> (cljs-env/with-compiler-env @compiler-env
           (cljsc/-compile
            [`(~'ns ~'cljs.user
               (:require ~@default-repl-requires))
             `(~'swap! ewen.replique.cljs-env.repl/connection ~'assoc :session ~(inc session))
             '(set! *print-fn* ewen.replique.cljs-env.repl/repl-print)
             '(set! *print-err-fn* ewen.replique.cljs-env.repl/repl-print)
             '(set! *print-newline* true)
             '(when (pos? (count ewen.replique.cljs-env.repl/print-queue))
                (ewen.replique.cljs-env.repl/flush-print-queue!))
             (init-core-bindings)]
            {}))
         make-eval-task
         (.submit new-executor))
    (try (.put conn-queue conn)
         (catch InterruptedException e
           (close-conn conn)))
    (swap! cljs-server assoc
           :state :ready)))

(defmethod dispatch-request :result [{:keys [content]} conn]
  (try 
    (.put (:result-queue @cljs-server)
          (read-string (:content content)))
    (.put (:conn-queue @cljs-server) conn)
    (catch InterruptedException e
      (close-conn conn))))

(defmethod dispatch-request :print [{:keys [content]} conn]
  ;; Maybe we should print only in the currently active REPL instead of all REPLs
  (doseq [[out out-lock] @cljs-outs]
    (binding [*out* out]
      (with-lock out-lock
        (-> (:content content) read-string print)
        (.flush *out*))))
  (try
    (send-and-close conn 200 "ignore__")
    (catch SocketException e nil)
    (catch InterruptedException e nil)
    (finally (.close conn))))

(comment
  (def tt (.submit (:executor @cljs-server) (make-eval-task "3")))
  (.get tt 1 TimeUnit/SECONDS)
  )








(defn can-write-file? [path]
  (let [f (File. path)]
    (and (.canWrite f) (not (.isDirectory f)))))

(defn port-number? [port-nb]
  (< -1 port-nb 65535))

(s/def ::cljs-env-type #{:browser :webapp})
(s/def ::output-to can-write-file?)
(s/def ::main string?)
(s/def ::compiler-opts (s/keys :req-un [::output-to] :opt-un [::main]))
(s/def ::port port-number?)
(s/def ::repl-opts (s/keys :req-un [::port]))
(defmulti cljs-env-type :cljs-env-type)
(defmethod cljs-env-type :browser [_]
  (s/keys :req-un [::cljs-env-type ::compiler-opts ::repl-opts]))
(defmethod cljs-env-type :webapp [_]
  (s/and (s/keys :req-un [::cljs-env-type ::compiler-opts ::repl-opts])
         #(contains? (:compiler-opts %) :main)))
(s/def ::cljs-env (s/multi-spec cljs-env-type :cljs-env-type))

(comment
  (s/conform ::cljs-env {:cljs-env-type :browser
                         :compiler-opts {:output-to "out/main.js"}
                         :repl-opts {:port 9001}})
  (s/explain ::cljs-env {:cljs-env-type :browser
                         :compiler-opts {:output-to "out/main.js"}
                         :repl-opts {:port 9001}})
  (s/explain-str ::cljs-env {:cljs-env-type :browser
                             :compiler-opts {:output-to "out/main.js"}
                             :repl-opts2 {:port 9001}})
  (s/explain-str ::cljs-env [])
  )

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

(defmulti init-opts :cljs-env-type)

(defn init-opts* [{{output-to :output-to} :compiler-opts
                   {port :port main :main} :repl-opts}]
  (let [output-dir (-> (io/file output-to) (.getAbsoluteFile) (.getParent))]
    {:compiler-opts (merge {:output-to (-> (io/file output-to) (.getAbsolutePath))
                            :output-dir output-dir
                            :optimizations :none
                            :recompile-dependents false
                            :preloads ['ewen.replique.cljs_env.repl
                                       'ewen.replique.cljs_env.browser]}
                           (when main {:main main}))
     :repl-opts {:analyze-path []
                 :host "127.0.0.1"
                 :port port}}))

(defmethod init-opts :browser [{{output-to :output-to} :compiler-opts :as opts}]
  (let [opts (init-opts* opts)
        output-dir (get-in opts [:compiler-opts :output-dir])]
    (update-in opts [:repl-opts] merge
               {:static-dir ["." output-dir]})))

(defmethod init-opts :webapp [opts]
  (init-opts* opts))

(defn load-javascript
  "Accepts a REPL environment, a list of namespaces, and a URL for a
  JavaScript file which contains the implementation for the list of
  namespaces. Will load the JavaScript file into the REPL environment
  if any of the namespaces have not already been loaded from the
  ClojureScript REPL."
  [repl-env provides url]
  (cljs.repl/-evaluate repl-env nil nil (slurp url)))

(defn evaluate-form [js]
  (let [{:keys [state executor port]} @cljs-server]
    (cond
      (= :stopped state)
      {:status :error
       :value "Cljs server stopped"}
      (= :starting state)
      {:status :error
       :value (format "Waiting for browser to connect on port %d ..." port)}
      :else (try (-> (.submit executor (make-eval-task js))
                     (.get))
                 (catch RejectedExecutionException e
                   {:status :error
                    :value "Connection broken"})))))

;; Defrecord instead of deftype because cljs.repl uses the repl-env as a hashmap. Why ??? 
(defrecord BrowserEnv [repl-opts]
  cljs.repl/IJavaScriptEnv
  (-setup [this opts] nil)
  (-evaluate [this _ _ js]
    (evaluate-form js))
  (-load [this provides url]
    (load-javascript this provides url))
  ;; We don't want the repl-env to be closed on cljs-repl exit
  (-tear-down [this] nil)
  cljs.repl/IReplEnvOptions
  (-repl-options [this] repl-opts)
  cljs.repl/IParseStacktrace
  (-parse-stacktrace [this st err opts]
    (st/parse-stacktrace this st err opts))
  cljs.repl/IGetError
  (-get-error [this e env opts]
    (edn/read-string
     (cljs.repl/evaluate-form this env "<cljs repl>"
                              `(when ~e
                                 (pr-str
                                  {:ua-product (clojure.browser.repl/get-ua-product)
                                   :value (str ~e)
                                   :stacktrace (.-stack ~e)}))))))

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
          (when (:main opts)
            (when-let [main (try (-> (:main opts) ana-api/parse-ns :ns)
                                 (catch Exception e nil))]
              (str "document.write('<script>if (typeof goog != \"undefined\") { goog.require(\"" (comp/munge main) "\"); } else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');\n"
                   "document.write('<script>if (typeof goog != \"undefined\") {ewen.replique.cljs_env.repl.connect(\"http://localhost:" port "\");} else { console.warn(\"ClojureScript could not load :main, did you forget to specify :asset-path?\"); };</script>');")))
          "})();\n"))))

(defn init-browser-env
  ([comp-opts repl-opts executor]
   (init-browser-env comp-opts repl-opts executor true))
  ([comp-opts repl-opts executor output-main-file?]
   (let [compiler-env* (-> comp-opts
                           closure/add-implicit-options
                           cljs-env/default-compiler-env)
         ;; Merge repl-opts in browserenv because clojurescript expects this. This is weird
         repl-env* (merge (BrowserEnv. repl-opts) repl-opts)]
     (cljs-env/with-compiler-env compiler-env*
       (comp/with-core-cljs nil
         (fn []
           (let [port (:port repl-opts)
                 repl-src "ewen/replique/cljs_env/repl.cljs"
                 benv-src "ewen/replique/cljs_env/browser.cljs"
                 repl-compiled (repl-compile-cljs repl-src comp-opts false)
                 benv-compiled (repl-compile-cljs benv-src comp-opts false)]
             (repl-cljs-on-disk
              repl-compiled (cljs.repl/-repl-options repl-env*) comp-opts)
             (repl-cljs-on-disk
              benv-compiled (cljs.repl/-repl-options repl-env*) comp-opts)
             (->> (refresh-cljs-deps comp-opts)
                  (closure/output-deps-file
                   (assoc comp-opts :output-to
                          (str (util/output-directory comp-opts)
                               File/separator "cljs_deps.js"))))
             (doto (io/file (util/output-directory comp-opts) "goog" "deps.js")
               util/mkdirs
               (spit (slurp (io/resource "goog/deps.js"))))
             (when output-main-file?
               (output-main-file comp-opts port))))))
     (reset! compiler-env compiler-env*)
     (reset! repl-env repl-env*))))

(defmethod server/tooling-msg-handle :shutdown [msg]
  (with-tooling-response msg
    (stop-cljs-server)
    {:shutdown true}))

(defn repl-caught [e repl-env opts]
  (binding [*out* server/tooling-err]
    (with-lock server/tooling-out-lock
      (-> {:type :eval
           :directory server/directory
           :error true
           :repl-type :cljs
           :session *session*
           :ns ana/*cljs-ns*
           :value (if (and (instance? IExceptionInfo e)
                           (#{:js-eval-error :js-eval-exception}
                            (:type (ex-data e))))
                    (:value (:error (ex-data e)))
                    (server/repl-caught-str e))}
          elisp/prn)))
  (cljs.repl/repl-caught e repl-env opts))

(defn cljs-repl []
  {:pre [(and (not (nil? @compiler-env))
              (not (nil? @repl-env))
              (not (= :stopped (:state @cljs-server))))]}
  (let [out-lock (ReentrantLock.)
        {:keys [state executor port]} @cljs-server]
    (swap! cljs-outs conj [*out* out-lock])
    (when (not= :ready state)
      (println (format "Waiting for browser to connect on port %d ..." port)))
    (apply
     (partial cljs.repl/repl @repl-env)
     (->> (merge
           (:options @@compiler-env)
           {:compiler-env @compiler-env
            :caught repl-caught
            :print (fn [result]
                     (binding [*out* server/tooling-out]
                       (with-lock server/tooling-out-lock
                         (elisp/prn {:type :eval
                                     :directory server/directory
                                     :repl-type :cljs
                                     :session *session*
                                     :ns ana/*cljs-ns*
                                     :result result})))
                     (with-lock out-lock
                       (println result)))
            ;; Code modifying the runtime should not be put in :init since it will be lost on
            ;; browser refresh
            :init (fn []
                    ;; Let the client know that we are entering a cljs repl
                    (binding [*out* server/tooling-out]
                      (with-lock server/tooling-out-lock
                        (elisp/prn {:type :eval
                                    :directory server/directory
                                    :repl-type :cljs
                                    :session *session*
                                    :ns ana/*cljs-ns*
                                    :result "nil"}))))})
          (apply concat)))
    (swap! cljs-outs disj [*out* out-lock])))

(defn set-cljs-env [{:keys [type cljs-env-opts] :as msg}]
  (let [cljs-env-opts (read-string cljs-env-opts)
        conformed-msg (s/conform ::cljs-env cljs-env-opts)]
    (if (= ::s/invalid conformed-msg)
      {:invalid (s/explain-str ::cljs-env conformed-msg)}
      (let [{:keys [compiler-opts repl-opts]} (init-opts conformed-msg)]
        (when @cljs-server (stop-cljs-server))
        (try
          (init-browser-env compiler-opts repl-opts (:executor @cljs-server))
          (start-cljs-server (:host repl-opts) (:port repl-opts))
          (catch Exception e
            (stop-cljs-server)
            (reset! compiler-env nil)
            (reset! repl-env nil)
            (throw e)))
        msg))))

(comment
  (server/tooling-msg-handle
   {:type :set-cljs-env
    :cljs-env-opts "{:cljs-env-type :browser
                    :compiler-opts {:output-to \"out/main.js\"}
                    :repl-opts {:port 9001}}"})
  
  )
