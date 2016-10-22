(ns ewen.replique.replique-conf
  (:require [ewen.replique.server :refer [with-tooling-response] :as server]
            [clojure.java.io :as io]
            [clojure.spec :as s]
            [clojure.spec.gen :as gen])
  (:import [java.io File]
           [java.nio.file Path Files LinkOption]))

(def ^:private cljs-server
  (delay (server/dynaload 'ewen.replique.server-cljs/cljs-server)))
(def ^:private start-cljs-server
  (delay (server/dynaload 'ewen.replique.server-cljs/start-cljs-server)))
(def ^:private stop-cljs-server
  (delay (server/dynaload 'ewen.replique.server-cljs/stop-cljs-server)))
(def ^:private init-browser-env
  (delay (server/dynaload 'ewen.replique.server-cljs/init-browser-env)))
(def ^:private compiler-env
  (delay (server/dynaload 'ewen.replique.server-cljs/compiler-env)))
(def ^:private repl-env
  (delay (server/dynaload 'ewen.replique.server-cljs/compiler-env)))

(def ^:dynamic *autocomplete* false)

(defn directory? [path]
  (and (string? path)
       (Files/isDirectory ^Path (.toPath (File. path)) (make-array LinkOption 0))))

(defn js-path? [path]
  (and (string? path)
       (let [p (.toPath (File. path))]
         (not (directory? path))
         (.endsWith path ".js"))))

(defn can-write-main-file? [path]
  (and string?
       (let [p (.toPath (File. path))]
         (and (not (directory? p))
              (Files/isReadable ^Path p)
              (js-path? path)
              (with-open [r (io/reader path)]
                (.contains (.readLine r) "** Replique main file **"))))))

(defn cljs-namespace? [path-or-sym]
  (if *autocomplete*
    (and string? path-or-sym
         (or (.endsWith path-or-sym ".cljs")
             (.endsWith path-or-sym ".cljc")))
    (symbol? path-or-sym)))

(def languages #{:ecmascript3 :ecmascript5 :ecmascript5-strict :ecmascript6-typed
                 :ecmascript6-strict :ecmascript6 :no-transpile})


(s/def ::cljs-env-type #{:browser :webapp})

(s/def ::module-type #{:commonjs :amd :es6})
(s/def ::foreign-lib-file js-path?)
(s/def ::foreign-lib-file-min js-path?)
(s/def ::provides (s/coll-of string?))
(s/def ::requires (s/coll-of string?))
(s/def ::foreign-lib (s/keys :req-un [::foreign-lib-file ::provides]
                             :opt-un [::foreign-lib-file-min ::requires
                                      ::module-type ::preprocess]))

(s/def ::source-map-path string?)
(s/def ::source-map-asset-path string?)
(s/def ::warnings (s/or :globally-enabled? boolean?
                        :warnings-map (s/map-of #{:preamble-missing :undeclared-var
                                                  :undeclared-ns :undeclared-ns-form
                                                  :redef :dynamic :fn-var :fn-arity
                                                  :fn-deprecated :protocol-deprecated
                                                  :undeclared-protocol-symbol
                                                  :invalid-protocol-symbol
                                                  :multiple-variadic-overloads
                                                  :variadic-max-arity :overload-arity
                                                  :extending-base-js-type :invoke-ctor
                                                  :invalid-arithmetic :protocol-invalid-method
                                                  :protocol-duped-method
                                                  :protocol-multiple-impls
                                                  :single-segment-namespace}
                                                boolean?)))
(s/def ::elide-asserts boolean?)
(s/def ::libs (s/* (s/alt :js-path js-path? :directory directory?)))
(s/def ::compiler-stats boolean?)
(s/def ::language-in languages)
(s/def ::language-out languages)
(s/def ::closure-warnings (s/map-of #{:access-controls :ambiguous-function-decl
                                      :debugger-statement-present :check-regexp
                                      :check-types :check-useless-code :check-variables
                                      :const :constant-property :deprecated :duplicate-message
                                      :es5-strict :externs-validation :fileoverview-jsdoc
                                      :global-this :internet-explorer-checks :invalid-casts
                                      :missing-properties :non-standard-jsdoc
                                      :strict-module-dep-check :tweaks :undefined-names
                                      :undefined-variables :unknown-defines :visiblity}
                                    #{:error :warning :off}))
(s/def ::closure-defines (s/map-of (s/or :string string? :symbol symbol?) (constantly true)))
(s/def ::closure-extra-annotations (s/coll-of string?))
(s/def ::anon-fn-naming-policy #{:off :unmapped :mapped})

(s/def ::output-to can-write-main-file?)
(s/def ::main cljs-namespace?)
(s/def ::asset-path string?)
(s/def ::verbose boolean?)
(s/def ::foreign-libs (s/coll-of ::foreign-lib))
(s/def ::preloads (s/coll-of symbol? :kind vector?))
(s/def ::compiler-opts (s/keys :req-un [::output-to]
                               :opt-un [::main ::asset-path ::verbose ::foreign-libs ::preloads
                                        ::source-map-path ::source-map-asset-path ::warnings
                                        ::elide-asserts ::libs ::compiler-stats ::language-in
                                        ::language-out ::closure-warnings ::closure-defines
                                        ::closure-extra-annotations ::anon-fn-naming-policy]))

(s/def ::repl-verbose boolean?)
(s/def ::port (s/int-in 0 65536))
(s/def ::repl-opts (s/keys :req-un [::port] :opt-un [::repl-verbose]))

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

(defmethod server/tooling-msg-handle :set-cljs-env [{:keys [type cljs-env-opts] :as msg}]
  (server/with-tooling-response msg
    (let [cljs-env-opts (read-string cljs-env-opts)
          conformed-msg (s/conform ::cljs-env cljs-env-opts)]
      (if (= ::s/invalid conformed-msg)
        {:invalid (s/explain-str ::cljs-env cljs-env-opts)}
        (let [{:keys [compiler-opts repl-opts]} (init-opts conformed-msg)
              {:keys [host port]} repl-opts]
          (when @@cljs-server (@stop-cljs-server))
          (try
            (@init-browser-env compiler-opts repl-opts (:executor @@cljs-server))
            (@start-cljs-server host port)
            (catch Exception e
              (@stop-cljs-server)
              (reset! @compiler-env nil)
              (reset! @repl-env nil)
              (throw e)))
          msg)))))

(comment
  (server/tooling-msg-handle
   {:type :set-cljs-env
    :directory server/directory
    :cljs-env-opts "{:cljs-env-type :browser
                    :compiler-opts {:output-to \"out/main.js\"}
                    :repl-opts {:port 9001}}"})
  
  )
