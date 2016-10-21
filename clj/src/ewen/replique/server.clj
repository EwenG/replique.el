(ns ewen.replique.server
  (:require [clojure.main]
            [clojure.core.server :refer [start-server *session*]]
            [clojure.java.io :refer [file]]
            [compliment.context :as context]
            [compliment.sources.local-bindings
             :refer [bindings-from-context]]
            [compliment.core :as compliment]
            [compliment.sources :as compliment-sources]
            [clojure.stacktrace :refer [print-stack-trace]]
            [ewen.replique.elisp-printer :as elisp])
  (:import [java.util.concurrent.locks ReentrantLock]
           [java.io File]))

(defonce directory nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (ReentrantLock.))
(defonce tooling-err nil)
(defonce watched-bindings
  [#'clojure.core/*data-readers* #'clojure.core/*print-namespace-maps*
   #'clojure.spec/*explain-out*  #'clojure.core/*print-level*
   #'clojure.core/*default-data-reader-fn* #'clojure.core/*print-length*
   #'clojure.core/*read-eval* #'clojure.core/*print-meta* #'clojure.core/*assert*
   #'clojure.core/*unchecked-math* #'clojure.core/*warn-on-reflection*
   #'clojure.core/*compile-path* #'clojure.core/*command-line-args*
   #'clojure.core/*math-context*])

(defn dynaload
  [s]
  (let [ns (namespace s)]
    (assert ns)
    (require (symbol ns))
    (let [v (resolve s)]
      (if v
        @v
        (throw (RuntimeException. (str "Var " s " is not on the classpath")))))))

(defmacro ^:private with-lock
  [lock-expr & body]
  `(let [lockee# ~(with-meta lock-expr
                    {:tag 'java.util.concurrent.locks.ReentrantLock})]
     (.lock lockee#)
     (try
       ~@body
       (finally
         (.unlock lockee#)))))

(defmacro with-tooling-response [msg & resp]
  `(let [type# (:type ~msg)
         directory# (:directory ~msg)]
     (try (merge {:type type# :directory directory#} ~@resp)
          (catch Exception t#
            {:directory directory#
             :type type#
             :error t#}))))

(defmacro with-err-str [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn repl-caught-str [e]
  (with-err-str (clojure.main/repl-caught e)))

(defmulti tooling-msg-handle :type)

(defn normalize-ip-address [address]
  (cond (= "0.0.0.0" address) "127.0.0.1"
        (= "0:0:0:0:0:0:0:1" address) "127.0.0.1"
        :else address))

#_(defn repl-infos []
  (let [server-infos (:replique @#'clojure.core.server/servers)]
    {:directory directory
     :replique
     {:host (-> (:socket server-infos)
                (.getInetAddress) (.getHostAddress) normalize-ip-address)
      :port (-> (:socket server-infos) (.getLocalPort))}}))

(defn start-repl-process [{:keys [port type cljs-env directory] :as opts}]
  (println "Starting Clojure REPL...")
  (try
    (alter-var-root #'directory (constantly directory))
    ;; Let leiningen :global-vars option propagate to other REPLs
    ;; The tooling REPL printing is a custom one and thus is not affected by those bindings,
    ;; and it must not !!
    (alter-var-root #'clojure.core.server/repl bound-fn*)
    (start-server {:port port :name :replique
                   :accept 'clojure.core.server/repl
                   :server-daemon false})
    (elisp/prn {:host (-> @#'clojure.core.server/servers
                          (get :replique) :socket
                          (.getInetAddress) (.getHostAddress)
                          normalize-ip-address)
                :port (-> @#'clojure.core.server/servers
                          (get :replique) :socket (.getLocalPort))
                :directory (.getAbsolutePath (file "."))})
    (catch Throwable t
      (elisp/prn {:error t}))))

(defn tooling-repl []
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :prompt #()
     :caught (fn [e]
               (repl-caught-str e))
     :print (fn [result]
              (elisp/prn result)))))

(comment
  (clojure.main/repl :prompt #())
  )

(defn shared-tooling-repl []
  (with-lock tooling-out-lock
    (alter-var-root #'tooling-out (constantly *out*)))
  (with-lock tooling-out-lock
    (alter-var-root #'tooling-err (constantly *err*)))
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (binding [*out* tooling-err]
         (with-lock tooling-out-lock
           (elisp/prn {:type :eval
                       :directory directory
                       :error true
                       :repl-type :clj
                       :thread (.getName thread)
                       :ns (ns-name *ns*)
                       :value (with-out-str (print-stack-trace ex))}))))))
  (let [init-fn (fn [] (in-ns 'ewen.replique.server))]
    (clojure.main/repl
     :init init-fn
     :prompt #()
     :caught (fn [e]
               (binding [*out* tooling-err]
                 (with-lock tooling-out-lock
                   (repl-caught-str e))))
     :print (fn [result]
              (with-lock tooling-out-lock
                (elisp/prn result))))))

(defn shutdown []
  (clojure.core.server/stop-servers))

(defmethod tooling-msg-handle :shutdown [msg]
  (with-tooling-response msg
    (shutdown)
    {:shutdown true}))

(comment
  (.start (Thread. (fn [] (throw (Exception. "e")))))
  )

(defn repl-eval
  "Enhanced :eval hook for saving bindings"
  [form]
  (let [prev-bindings (mapv deref watched-bindings)
        evaled (eval form)
        next-bindings (mapv deref watched-bindings)]
    (doseq [[prev-b next-b v] (map vector prev-bindings next-bindings watched-bindings)
            :when (not (identical? prev-b next-b))]
      (let [{v-name :name v-ns :ns} (meta v)]
        (when (and v-name v-ns)
          (binding [*out* tooling-err]
            (with-lock tooling-out-lock
              (elisp/prn {:type :binding
                          :directory directory
                          :repl-type :clj
                          :session *session*
                          :ns (ns-name *ns*)
                          :var (symbol (str v-ns) (str v-name))
                          :value (pr-str @v)}))))))
    evaled))

(defn repl []
  (println "Clojure" (clojure-version))
  (clojure.main/repl
   :init clojure.core.server/repl-init
   :eval repl-eval
   :caught (fn [e]
             (binding [*out* tooling-err]
               (with-lock tooling-out-lock
                 (elisp/prn {:type :eval
                             :directory directory
                             :error true
                             :repl-type :clj
                             :session *session*
                             :ns (ns-name *ns*)
                             :value (repl-caught-str e)})))
             (clojure.main/repl-caught e))
   :print (fn [result]
            (binding [*out* tooling-out]
              (with-lock tooling-out-lock
                (elisp/prn {:type :eval
                            :directory directory
                            :repl-type :clj
                            :session *session*
                            :ns (ns-name *ns*)
                            :result (pr-str result)})))
            (prn result))))

(defn format-meta [{:keys [file] :as meta} keys]
  (let [f (and file (File. file))]
    (if (and f (.exists f))
      (select-keys (assoc
                    meta :file
                    (.getAbsolutePath f))
                   keys)
      (select-keys meta (disj keys :file :line :column)))))

(defmethod tooling-msg-handle :clj-var-meta
  [{:keys [context ns symbol keys] :as msg}]
  (with-tooling-response msg
    (let [ctx (when context (read-string context))
          ctx (context/parse-context ctx)
          bindings (bindings-from-context ctx)
          keys (into #{} keys)]
      (cond
        (or (nil? ns) (nil? symbol) (nil? (find-ns ns)))
        {:meta nil}
        (and ctx (contains? (into #{} bindings) (name symbol)))
        {:not-found :local-binding}
        :else
        (let [v (when (symbol? symbol)
                  (try (ns-resolve ns symbol)
                       (catch ClassNotFoundException e
                         nil)))
              meta (when (and v (meta v))
                     (format-meta (meta v) keys))]
          (if (empty? meta)
            {:meta nil}
            {:meta meta}))))))

(comment
  (let [tooling-msg-handle "e"]
    tooling-msg-handle))

(comment
  (tooling-msg-handle {:type :clj-var-meta
                       :context nil
                       :ns 'ewen.replique.server
                       :symbol 'tooling-msg-handle
                       :keys '(:column :line :file)})

  (tooling-msg-handle {:type :clj-var-meta
                       :context nil
                       :ns 'compliment.core
                       :symbol 'all-sources
                       :keys '(:column :line :file)})

  (tooling-msg-handle {:type :clj-var-meta
                       :context nil
                       :ns 'ewen.foo
                       :symbol 'foo-bar
                       :keys '(:column :line :file)})

  )
