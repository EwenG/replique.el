(ns ewen.replique.lein
  (:require [clojure.walk :as walk]))

(defn- unquote-project
  "Inside defproject forms, unquoting (~) allows for arbitrary evaluation."
  [args]
  (walk/walk (fn [item]
               (cond (and (seq? item) (= `unquote (first item)))
                     (second item)
                     ;; needed if we want fn literals preserved
                     (or (seq? item) (symbol? item)) (list 'quote item)
                     :else (let [result (unquote-project item)]
                             ;; clojure.walk strips metadata
                             (if-let [m (meta item)]
                               (with-meta result m)
                               result))))
             identity
             args))

(defn- argument-list->argument-map
  [args]
  (let [keys (map first (partition 2 args))
        unique-keys (set keys)]
    (if (= (count keys) (count unique-keys))
      (apply hash-map args)
      (let [duplicates (->> (frequencies keys)
                            (remove #(> 2 (val %)))
                            (map first))]
        (throw
         (IllegalArgumentException.
          (format "Duplicate keys: %s"
                  (clojure.string/join ", " duplicates))))))))

(defmacro defproject
  "The project.clj file must either def a project map or call this macro.
  See `lein help sample` to see what arguments it accepts."
  [project-name version & args]
  `(let [args# ~(-> (argument-list->argument-map args)
                    unquote-project
                    (select-keys [:resource-paths
                                  :source-paths
                                  :dependencies]))]
     (def ~'project args#)))

(defn read-raw
  "Read project file"
  [file]
  (locking read-raw
    (binding [*ns* (find-ns 'ewen.replique.lein)]
      (try (load-file file)
           (catch Exception e
             (throw (Exception. (format "Error loading %s" file) e)))))
    (let [project (resolve 'ewen.replique.lein/project)]
      (when-not project
        (throw (Exception. (format "%s must define project map" file))))
      ;; return it to original state
      (ns-unmap 'ewen.replique.lein 'project)
      @project)))
