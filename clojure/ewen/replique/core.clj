(ns ewen.replique.core)

(defrecord ToolingMsg [type result])

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle "load-file"
  [{:keys [type file-path load-file-fn]
    :or {load-file-fn load-file}}]
  (map->ToolingMsg
   {:type type
    :result (pr-str (load-file-fn file-path))}))

(defmethod tooling-msg-handle "set-ns"
  [{:keys [type ns in-ns-fn]
    :or {in-ns-fn in-ns}}]
  (map->ToolingMsg
   {:type type
    :result (pr-str (-> ns symbol in-ns-fn pr-str))}))

#_(defmethod tooling-msg-handle "completions"
  [{:keys [type prefix ns] :as msg}]
  (map->ToolingMsg
   {:type type
    :result (ewen.replique.compliment.core/completions
             prefix (select-keys msg [:ns]))}))

(defmethod tooling-msg-handle "completions"
  [{:keys [type prefix] :as msg}]
  (map->ToolingMsg
   {:type type
    :result (pr-str "e")}))
