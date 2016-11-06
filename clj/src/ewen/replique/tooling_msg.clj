(ns ewen.replique.tooling-msg
  (:require [ewen.replique.utils :as utils]
            [ewen.replique.elisp-printer :as elisp]
            [clojure.stacktrace :refer [print-stack-trace]])
  (:import [java.util.concurrent.locks ReentrantLock]))

(defonce directory nil)
(defonce tooling-out nil)
(defonce tooling-out-lock (ReentrantLock.))
(defonce tooling-err nil)
(defonce process-out nil)
(defonce process-out-lock (ReentrantLock.))
(defonce process-err nil)

(defmulti tooling-msg-handle :type)

(defmacro with-tooling-response [msg & resp]
  `(let [type# (:type ~msg)
         directory# (:directory ~msg)]
     (try (merge {:type type# :directory directory#} (~'do ~@resp))
          (catch Exception t#
            {:directory directory#
             :type type#
             :error t#}))))

(defn uncaught-exception [thread ex]
  (if (or (nil? tooling-err) (nil? tooling-out-lock))
    (throw ex)
    (binding [*out* tooling-err]
      (utils/with-lock tooling-out-lock
        (elisp/prn {:type :eval
                    :directory directory
                    :error true
                    :repl-type :clj
                    :thread (.getName thread)
                    :ns (ns-name *ns*)
                    :value (with-out-str (print-stack-trace ex))})))))
