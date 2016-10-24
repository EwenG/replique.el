(ns ewen.replique.tooling-msg)

(defmulti tooling-msg-handle :type)

(defmacro with-tooling-response [msg & resp]
  `(let [type# (:type ~msg)
         directory# (:directory ~msg)]
     (try (merge {:type type# :directory directory#} ~@resp)
          (catch Exception t#
            {:directory directory#
             :type type#
             :error t#}))))
