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
  `(let [type# (:type ~msg)]
     (try (merge {:type type#} ~@resp)
          (catch Throwable t#
            {:type type#
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

(defn repl-infos []
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
      (prn {:error t}))))

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

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (binding [*out* tooling-err]
       (with-lock tooling-out-lock
         (elisp/prn {:type :eval
                     :error true
                     :repl-type :clj
                     :thread (.getName thread)
                     :ns (ns-name *ns*)
                     :value (with-out-str (print-stack-trace ex))}))))))

(comment
  (.start (Thread. (fn [] (throw (Exception. "e")))))
  )

(defn repl []
  (println "Clojure" (clojure-version))
  (clojure.main/repl
   :init clojure.core.server/repl-init
   :caught (fn [e]
             (binding [*out* tooling-err]
               (with-lock tooling-out-lock
                 (elisp/prn {:type :eval
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
                            :repl-type :clj
                            :session *session*
                            :ns (ns-name *ns*)
                            :result (pr-str result)})))
            (prn result))))

(defmethod tooling-msg-handle :set-cljs-env [msg]
  (with-tooling-response msg
    (require 'ewen.replique.server-cljs)
    ((ns-resolve 'ewen.replique.server-cljs 'set-cljs-env) msg)))

(defn cljs-repl []
  (require 'ewen.replique.server-cljs)
  ((ns-resolve 'ewen.replique.server-cljs 'cljs-repl)))

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
