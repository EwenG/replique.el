(ns ewen.replique.core)

(defrecord ToolingMsg [type platform result])

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle "load-file"
  [{:keys [type file-path load-file-fn platform]
    :or {load-file-fn load-file
         platform "clj"}}]
  (try (map->ToolingMsg
        {:type type
         :platform platform
         :result (pr-str (load-file-fn file-path))})
       (catch Throwable t
         (map->ToolingMsg
          {:type type
           :platform platform
           :result nil
           :error t}))))

(defmethod tooling-msg-handle "set-ns"
  [{:keys [type ns in-ns-fn platform]
    :or {in-ns-fn in-ns
         platform "clj"}}]
  (try
    (map->ToolingMsg
     {:type type
      :platform platform
      :result (pr-str (-> ns symbol in-ns-fn pr-str))})
    (catch Throwable t
      (map->ToolingMsg
       {:type type
        :platform platform
        :result nil
        :error t}))))

(defmethod tooling-msg-handle "completions"
  [{:keys [type prefix completion-fn platform]
    :or {completion-fn ewen.replique.completion/completions
         platform "clj"}
    :as msg}]
  (try (map->ToolingMsg
        {:type type
         :platform platform
         :result (completion-fn
                  prefix (select-keys msg [:ns]))})
       (catch Throwable t
         (map->ToolingMsg
          {:type type
           :platform platform
           :result nil
           :error t}))))

(defmethod tooling-msg-handle "add-classpath"
  [{:keys [type path classloader-hierarchy-fn platform]
    :or {classloader-hierarchy-fn
         (partial ewen.replique.classpath/classloader-hierarchy
                  (.. clojure.lang.RT baseLoader))
         platform "clj"}
    :as msg}]
  (try (map->ToolingMsg
        {:type type
         :platform platform
         :result (-> (classloader-hierarchy-fn)
                     ewen.replique.classpath/choose-classloader
                     (ewen.replique.classpath/add-classpath path))})
       (catch Throwable t
         (map->ToolingMsg
          {:type type
           :platform platform
           :result nil
           :error t}))))
