(ns ewen.replique.core)

(defrecord ToolingMsg [type result])

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle "load-file"
  [{:keys [type file-path load-file-fn]
    :or {load-file-fn load-file}}]
  (try (map->ToolingMsg
        {:type type
         :result (pr-str (load-file-fn file-path))})
       (catch Exception e
         (map->ToolingMsg
          {:type type
           :result nil
           :error e}))))

(defmethod tooling-msg-handle "set-ns"
  [{:keys [type ns in-ns-fn]
    :or {in-ns-fn in-ns}}]
  (try
    (map->ToolingMsg
     {:type type
      :result (pr-str (-> ns symbol in-ns-fn pr-str))})
    (catch Exception e
      (map->ToolingMsg
       {:type type
        :result nil
        :error e}))))

(defmethod tooling-msg-handle "completions"
  [{:keys [type prefix completion-fn]
    :or {completion-fn ewen.replique.completion/completions}
    :as msg}]
  (try (map->ToolingMsg
        {:type type
         :result (completion-fn
                  prefix (select-keys msg [:ns]))})
       (catch Exception e
         (map->ToolingMsg
          {:type type
           :result nil
           :error e}))))
