(ns ewen.replique.core)

(defrecord ToolingMsg [type result])

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle "load-file"
  [{:keys [type file-path]}]
  (map->ToolingMsg
   {:type type
    :result (pr-str (load-file file-path))}))

(defmethod tooling-msg-handle "set-ns"
  [{:keys [type ns]}]
  (map->ToolingMsg
   {:type type
    :result (pr-str (-> ns symbol in-ns pr-str))}))

(defmethod tooling-msg-handle "completions"
  [{:keys [type prefix ns] :as msg}]
  (map->ToolingMsg
   {:type type
    :result (ewen.replique.compliment.core/completions
             prefix (select-keys msg [:ns]))}))
