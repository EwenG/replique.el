(ns ewen.replique.server-clj
  (:require [ewen.replique.server :refer [with-tooling-response]
             :as server]
            [clojure.core.server :refer [start-server]]
            [clojure.java.io :as io :refer [file]]))

(defmethod server/repl-dispatch [:clj nil]
  [{:keys [port type cljs-env directory] :as opts}]
  (try
    (alter-var-root #'server/directory (constantly directory))
    (start-server {:port port :name :replique
                   :accept 'clojure.core.server/repl
                   :server-daemon false})
    (doto (file ".replique-port")
      (spit (str {:repl (-> @#'clojure.core.server/servers
                            (get :replique) :socket (.getLocalPort))}))
      (.deleteOnExit))
    (prn {:host (-> @#'clojure.core.server/servers
                    (get :replique) :socket
                    (.getInetAddress) (.getHostAddress)
                    server/normalize-ip-address)
          :port (-> @#'clojure.core.server/servers
                    (get :replique) :socket (.getLocalPort))
          :directory (.getAbsolutePath (java.io.File. "."))})
    (catch Throwable t
      (prn {:error t}))))

#_(defmethod server/tooling-msg-handle :repl-infos [msg]
  (server/with-tooling-response msg
    (assoc (server/repl-infos) :repl-type :clj)))

(defmethod server/tooling-msg-handle :shutdown [msg]
  (server/with-tooling-response msg
    (server/shutdown)
    {:shutdown true}))
