(ns ewen.replique.core
  (:require [clojure.java.io :as io]
            [clojure.set])
  (:import [java.net URI]))

(defrecord ToolingMsg [type platform result])

(defmacro with-tooling-response [msg platform & body]
  `(try (map->ToolingMsg
         {:type (:type ~msg)
          :platform ~platform
          :result ~@body})
        (catch Throwable t#
          (map->ToolingMsg
           {:type (:type ~msg)
            :platform ~platform
            :result nil
            :error t#}))))

(defmulti tooling-msg-handle :type)

(defmethod tooling-msg-handle "load-file"
  [{:keys [file-path] :as msg}]
  (with-tooling-response msg "clj"
    (pr-str (load-file file-path))))

(defmethod tooling-msg-handle "set-ns"
  [{:keys [ns] :as msg}]
  (with-tooling-response msg "clj"
    (pr-str (-> ns symbol in-ns pr-str))))

(defmethod tooling-msg-handle "completions"
  [{:keys [prefix ns] :as msg}]
  (with-tooling-response msg "clj"
    (ewen.replique.completion/completions
     prefix (select-keys msg [:ns]))))

(defmethod tooling-msg-handle "add-classpath"
  [{:keys [path] :as msg}]
  (with-tooling-response msg "clj"
    (-> (ewen.replique.classpath/classloader-hierarchy
         (.. clojure.lang.RT baseLoader))
        ewen.replique.classpath/choose-classloader
        (ewen.replique.classpath/add-classpath path))))

(defn reload-project [classloader-hierarchy file-path]
  (let [path->url-str (fn [path]
                        (-> (io/file path)
                            .toURI
                            .toURL
                            .toString))
        cp-urls
        (ewen.replique.classpath/get-classpath
         classloader-hierarchy)
        {:keys [resource-paths source-paths]}
        (ewen.replique.lein/read-raw file-path)
        urls (->> (concat resource-paths source-paths)
                  (map path->url-str))
        urls (clojure.set/difference (set urls)
                                     (set cp-urls))
        cl (-> classloader-hierarchy
               ewen.replique.classpath/choose-classloader)]
    (doseq [url urls]
      (ewen.replique.classpath/add-classpath
       cl (URI. url)))
    urls))

(defmethod tooling-msg-handle "reload-project"
  [{:keys [file-path] :as msg}]
  (with-tooling-response msg "clj"
    (reload-project (ewen.replique.classpath/classloader-hierarchy
                     (.. clojure.lang.RT baseLoader))
                    file-path)))
