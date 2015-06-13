(ns ewen.replique.core)

(defrecord ToolingMsg [type result])

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle "load-file"
  [{:keys [type file-path load-file-fn]
    :or {load-file-fn load-file}}]
  (try (map->ToolingMsg
        {:type type
         :result (pr-str (load-file-fn file-path))})
       (catch Throwable t
         (map->ToolingMsg
          {:type type
           :result nil
           :error t}))))

(defmethod tooling-msg-handle "set-ns"
  [{:keys [type ns in-ns-fn]
    :or {in-ns-fn in-ns}}]
  (try
    (map->ToolingMsg
     {:type type
      :result (pr-str (-> ns symbol in-ns-fn pr-str))})
    (catch Throwable t
      (map->ToolingMsg
       {:type type
        :result nil
        :error t}))))

(defmethod tooling-msg-handle "completions"
  [{:keys [type prefix completion-fn]
    :or {completion-fn ewen.replique.completion/completions}
    :as msg}]
  (try (map->ToolingMsg
        {:type type
         :result (completion-fn
                  prefix (select-keys msg [:ns]))})
       (catch Throwable t
         (map->ToolingMsg
          {:type type
           :result nil
           :error t}))))

(defmethod tooling-msg-handle "add-classpath"
  [{:keys [type path classloader-hierarchy-fn]
    :or {classloader-hierarchy-fn
         (partial ewen.replique.classpath/classloader-hierarchy
                  (.. clojure.lang.RT baseLoader))}
    :as msg}]
  (try (map->ToolingMsg
        {:type type
         :result (-> (classloader-hierarchy-fn)
                     ewen.replique.classpath/choose-classloader
                     (ewen.replique.classpath/add-classpath path))})
       (catch Throwable t
         (map->ToolingMsg
          {:type type
           :result nil
           :error t}))))
