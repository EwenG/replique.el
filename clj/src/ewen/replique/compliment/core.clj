;; ## Compliment - a completion library you deserve.
;; This library provides a fast and extensible way to complete symbols in your
;; editor. It is intended to be maximally editor-agnostic where
;; possible, to avoid duplicating implementation in different clients.

(ns ewen.replique.compliment.core
  "Core namespace. Most interactions with Compliment should happen
  through functions defined here."
  (:require [ewen.replique.compliment.sources ns-mappings
             namespaces-and-classes class-members keywords
             special-forms local-bindings resources]
            [ewen.replique.namespace :as replique-ns])
  (:use [ewen.replique.compliment.sources :only [all-sources]]
        [ewen.replique.compliment.context :only [cache-context]]
        [ewen.replique.compliment.utils :refer [*extra-metadata*]]
        [clojure.string :only [join]])
  (:import java.util.Comparator))

(def ^:private by-length-comparator
  (reify Comparator
    (compare [_ s1 s2]
      (let [res (compare (count s1) (count s2))]
        (if (zero? res)
          (compare s1 s2)
          res)))))

(defn sort-by-length
  "Sorts list of strings by their length first, and then alphabetically if
  length is equal. Works for tagged and non-tagged results."
  [candidates]
  (sort-by :candidate by-length-comparator candidates))

(defn ensure-ns
  "Takes either a namespace object or a symbol and returns the corresponding
  namespace if it exists, otherwise returns `user` namespace."
  [ns cljs-comp-env]
  (cond (instance? clojure.lang.Namespace ns) ns
        (symbol? ns) (or (replique-ns/find-ns ns cljs-comp-env)
                         (if cljs-comp-env
                           (replique-ns/find-ns 'cljs.user cljs-comp-env)
                           (replique-ns/find-ns 'user nil)))
        :else *ns*))

(defn completions
  "Returns a list of completions for the given prefix. Options map can contain
  the following options:
  - :ns - namespace where completion is initiated;
  - :context - code form around the prefix;
  - :sort-order (either :by-length or :by-name);
  - :plain-candidates - if true, returns plain strings instead of maps;
  - :extra-metadata - set of extra fields to add to the maps;
  - :sources - list of source keywords to use;
  - :cljs-comp-env - the cljs compilation environment to be used when
  completion is requested from clojurescript."
  ([prefix]
   (completions prefix {}))
  ([prefix options-map]
   (if (string? options-map)
     (completions prefix {:context options-map})
     (let [{:keys [ns context sort-order sources extra-metadata cljs-comp-env]
            :or {sort-order :by-length}} options-map
           ns (ensure-ns ns cljs-comp-env)
           options-map (assoc options-map :ns ns)
           ctx (cache-context context)
           sort-fn (if (= sort-order :by-name)
                     (partial sort-by :candidate)
                     (partial sort-by-length true))]
       (binding [*extra-metadata* extra-metadata]
         (let [candidate-fns (keep (fn [[_ src]]
                                     (when (:enabled src)
                                       (:candidates src)))
                                   (if sources
                                     (all-sources sources)
                                     (all-sources)))]
           (as-> (mapcat (fn [f] (if cljs-comp-env
           (f prefix ns ctx cljs-comp-env)
           (f prefix ns ctx)))
           candidate-fns)
               candidates

             (if (= sort-order :by-name)
               (sort-by :candidate candidates)
               (sort-by :candidate by-length-comparator candidates))

             (if (:plain-candidates options-map)
               (map :candidate candidates)
               candidates)
             (doall candidates))))))))

(defn documentation
  "Returns a documentation string that describes the given symbol."
  ([symbol-str]
   (documentation symbol-str *ns*))
  ([symbol-str ns]
   (if (empty? symbol-str)
     ""
     (->> (for [[_ {:keys [doc enabled]}] (all-sources)
                :when enabled
                :let [docstr (doc symbol-str (ensure-ns ns))]
                :when docstr]
            docstr)
          (interpose "\n\n")
          join))))
