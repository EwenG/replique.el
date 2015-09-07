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
  (:import [java.io File]
           [java.net URL]
           [java.util Base64]
           [java.nio.charset StandardCharsets]))

(defn decodeBase64 [s]
  (let [b (.getBytes s StandardCharsets/UTF_8)]
    (-> (Base64/getDecoder)
        (.decode b)
        (String.))))

(defn encodeBase64 [s]
  (let [b (.getBytes s StandardCharsets/UTF_8)]
    (-> (Base64/getEncoder)
        (.encode b)
        (String.))))

(when (not (find-ns 'ewen.replique.core))
  (create-ns 'ewen.replique.core)
  (intern 'ewen.replique.core 'tooling-msg-handle nil)
  (intern 'ewen.replique.core 'map->ToolingMsg nil)
  (intern 'ewen.replique.core 'reload-project nil))
(when (not (find-ns 'ewen.replique.completion))
  (create-ns 'ewen.replique.completion)
  (intern 'ewen.replique.completion 'completions nil))
(when (not (find-ns 'ewen.replique.classpath))
  (create-ns 'ewen.replique.classpath)
  (intern 'ewen.replique.classpath 'classloader-hierarchy nil))

(defonce compiler-env (atom nil))
(defonce repl-env (atom nil))
(defonce env {:context :expr :locals {}})

;; The cljs REPL uses "print" to print values evaluated in the
;; browser and returned by the browser after a call to "pr-str".
;; This is why clojurescript ToolingMsg are wrapped in a "pr-str"
;; call.

(defmacro with-tooling-response [msg platform & body]
  `(pr-str
    (try (ewen.replique.core/map->ToolingMsg
          {:type (:type ~msg)
           :uid (:uid ~msg)
           :platform ~platform
           :result ~@body})
         (catch Throwable t#
           (ewen.replique.core/map->ToolingMsg
            {:type (:type ~msg)
             :uid (:uid ~msg)
             :platform ~platform
             :result nil
             :error t#})))))

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
                      (into js-files))
        js-files (deps/dependency-order js-files)]
    (closure/output-deps-file
     (assoc opts :output-to
            (str (util/output-directory opts)
                 File/separator "cljs_deps.js"))
     js-files)))

(defn cljs-in-ns [repl-env env ns-name]
  (let [ns-name (symbol ns-name)]
    (when-not (ana/get-namespace ns-name)
      (swap! cljs-env/*compiler* assoc-in
             [::ana/namespaces ns-name]
             {:name ns-name})
      (cljs.repl/-evaluate
       repl-env "<cljs repl>" 1
       (str "goog.provide('" (comp/munge ns-name) "');")))
    (set! ana/*cljs-ns* ns-name)))

(defmulti load-file-generic
  (fn [repl-env env opts {:keys [file-type] :as msg} ]
    file-type))

(defmethod load-file-generic "js"
  [repl-env env opts {:keys [file-path] :as msg}]
  (cljs.repl/-evaluate repl-env "<cljs repl>" 1 (slurp file-path)))

(defmethod load-file-generic "css"
  [repl-env env opts {:keys [file-path css-file] :as msg}]
  (-> (cljs.repl/-evaluate
       repl-env "<cljs repl>" 1
       (format "ewen.replique.cljs_env.browser.reload_css(%s, %s);"
               (pr-str css-file)
               (-> (slurp file-path)
                   encodeBase64
                   pr-str)))
      :value))

(defmulti tooling-msg-handle
  (fn [repl-env env [_ {:keys [type]}] opts]
    type))

(defn f->src [f]
  (cond (util/url? f) f
        (.exists (io/file f)) (io/file f)
        :else (io/resource f)))

(defn repl-compile-cljs [repl-env f opts]
  (let [src (f->src f)
        compiled (binding [ana/*reload-macros* true]
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
    compiled))



(defn repl-cljs-on-disk [compiled repl-env opts]
  (let [sources (closure/add-dependencies
                 (merge
                  (#'cljs.repl/env->opts repl-env) opts)
                 compiled)]
    (doseq [source sources]
      (closure/source-on-disk opts source))))

(defn repl-eval-compiled [compiled repl-env f opts]
  (let [src (f->src f)]
    (cljs.repl/-evaluate
     repl-env "<cljs repl>" 1
     (slurp (str (util/output-directory opts)
                 File/separator "cljs_deps.js")))
    (cljs.repl/-evaluate
     repl-env f 1 (closure/add-dep-string opts compiled))
    (cljs.repl/-evaluate
     repl-env f 1
     (closure/src-file->goog-require
      src {:wrap true :reload true :macros-ns (:macros-ns compiled)}))))

(defmethod tooling-msg-handle "load-file"
  [repl-env env [_ {:keys [file-path] :as msg}] opts]
  (with-tooling-response msg "cljs"
    (if (.endsWith file-path ".clj")
      ;;Handle macro reloading
      (load-file file-path)
      (cljs-env/with-compiler-env @compiler-env
        (let [compiled (repl-compile-cljs repl-env file-path opts)]
          (repl-cljs-on-disk compiled repl-env opts)
          (refresh-cljs-deps opts)
          (repl-eval-compiled compiled repl-env file-path opts))
        file-path))))

(defmethod tooling-msg-handle "set-ns"
  [repl-env env [_ {:keys [ns] :as msg}] opts]
  (with-tooling-response msg "cljs"
    (cljs-in-ns repl-env env (symbol ns))))

(defmethod tooling-msg-handle "completions"
  [repl-env env [_ {:keys [prefix ns] :as msg}] opts]
  (with-tooling-response msg "cljs"
    (ewen.replique.completion/completions
     prefix (assoc (select-keys msg [:ns])
                   :cljs-comp-env
                   @cljs-env/*compiler*))))

(defmethod tooling-msg-handle "add-classpath"
  [repl-env env [_ msg] opts]
  (pr-str (with-tooling-response msg "cljs"
            (ewen.replique.classpath/classloader-hierarchy
             (.. Thread currentThread
                 getContextClassLoader)))))

(defmethod tooling-msg-handle "reload-project"
  [repl-env env [_ {:keys [file-path] :as msg}] opts]
  (with-tooling-response msg "cljs"
    (ewen.replique.core/reload-project
     (ewen.replique.classpath/classloader-hierarchy
      (.. Thread currentThread
          getContextClassLoader))
     file-path)))

(defmethod tooling-msg-handle "list-css"
  [repl-env env [_ msg] opts]
  (with-tooling-response msg "cljs"
    (-> (cljs.repl/-evaluate
         repl-env "<cljs repl>" 1
         "ewen.replique.cljs_env.browser.list_css_infos();")
        :value
        read-string)))

(defn url->out-dir-file [url out-dir]
  (File. out-dir (.getParh url)))

(defmethod tooling-msg-handle "list-sass"
  [repl-env env [_ msg] {:keys [output-dir]}]
  (with-tooling-response msg "cljs"
    (let [css-paths
          (->
           (cljs.repl/-evaluate
            repl-env "<cljs repl>" 1
            "ewen.replique.cljs_env.browser.list_css_stylesheet_paths();")
           :value
           read-string)
          css-urls (map #(URL. %) css-paths)
          {css-http "http" css-file "file"}
          (group-by #(.getProtocol %) css-urls)
          css-resources (map #(File. output-dir (.getPath %)) css-http)]
      (prn (map #(.getPath %) css-http))
      (prn output-dir)
      css-paths)))

(defmethod tooling-msg-handle "load-file-generic"
  [repl-env env [_ msg] opts]
  (with-tooling-response msg "cljs"
    (load-file-generic repl-env env opts msg)))

(defmethod tooling-msg-handle "eval-form"
  [repl-env env [_ {:keys [form] :as msg}] opts]
  (with-tooling-response msg "cljs"
    (binding [*default-data-reader-fn* (fn [t v] v)]
      (-> (#'cljs.repl/eval-cljs repl-env env (read-string form) opts)
          read-string))))

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
    (reset! compiler-env (-> opts
                             closure/add-implicit-options
                             cljs-env/default-compiler-env))
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
