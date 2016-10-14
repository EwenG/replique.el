(ns ewen.replique.spec-tooling
  (:refer-clojure :exclude [+ * and assert or cat def keys merge])
  (:require [clojure.spec :as s]
            [ewen.replique.replique-conf :as conf]
            [clojure.pprint :refer [pprint pp]]
            [clojure.walk :as walk]))

(alias 'c 'clojure.core)

(declare candidates)
(declare form->spec)

(defprotocol Complete
  (candidates* [spec context prefix]))

(extend-protocol Complete
  clojure.lang.Var
  (candidates* [v context prefix]
    @v))

(extend-protocol Complete
  clojure.spec.Spec
  (candidates* [spec context prefix]
    (candidates (-> spec s/form form->spec) context prefix)))

(extend-protocol Complete
  clojure.lang.Keyword
  (candidates* [k context prefix]
    (when-let [spec (s/get-spec k)]
      (candidates (-> spec s/form form->spec) context prefix))))

(defn set-candidates [s context prefix]
  (when (c/nil? context)
    (->> (filter #(.startsWith (str %) prefix) s)
         (into #{}))))

(extend-protocol Complete
  clojure.lang.IFn
  (candidates* [f context prefix]
    (if (instance? clojure.lang.IPersistentSet f)
      (set-candidates f context prefix)
      f)))

(extend-protocol Complete
  nil
  (candidates* [spec context prefix] nil))

(extend-protocol Complete
  Object
  (candidates* [spec context prefix] nil))

(defn complete? [x]
  (when (instance? ewen.replique.spec_tooling.Complete x) x))

(defn cat-form->spec [key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        ps (mapv (comp form->spec second) pairs)]
    {::s/op ::s/pcat :ps ps}))

(defn alt-form->spec [key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        ps (mapv (comp form->spec second) pairs)]
    {::s/op ::s/alt :ps ps}))

(defn rep-form->spec [pred]
  {::s/op ::s/rep :p1 (form->spec pred)})

(defn amp-form->spec [pred preds]
  {::s/op ::s/amp :p1 (form->spec pred) :ps (mapv form->spec preds)})

;; Form is a spec OR form is a qualified symbol and can be resolved to a var OR form is something
;; that get evaled in order to get the values than the ones from core.spec spec impls.
;; Qualified symbols are not evaled in order to customize the candidates* behavior for vars
;; It is resolved to a var and not kept as a symbol, otherwise reg-resolve would try to resolve
;; it to a spec
(defn form->spec [form]
  (when form
    (cond (seq? form)
          (let [[spec-sym & spec-rest] form]
            (cond (= 'clojure.spec/cat spec-sym)
                  (cat-form->spec spec-rest)
                  (= 'clojure.spec/alt spec-sym)
                  (alt-form->spec spec-rest)
                  (= 'clojure.spec/* spec-sym)
                  (rep-form->spec (first spec-rest))
                  (= 'clojure.spec/& spec-sym)
                  (amp-form->spec (first spec-rest) (drop 1 spec-rest))
                  :else (eval form)))
          (c/and (symbol? form) (namespace form) (var? (resolve form)))
          (resolve form)
          :else (eval form))))

(defprotocol Specize
  (specize* [_] [_ form]))

(declare regex-spec-impl)

(defn- reg-resolve [k]
  (if (ident? k)
    (let [reg @@#'s/registry-ref
          spec (get reg k)]
      (when spec
        (if-not (ident? spec)
          (-> spec s/form form->spec)
          (-> (#'s/deep-resolve reg spec) s/form form->spec))))
    k))

(defn- reg-resolve! [k]
  (if (ident? k)
    (c/or (reg-resolve k)
          (throw (Exception. (str "Unable to resolve spec: " k))))
    k))

(defn- and-all [preds x]
  (loop [[p & ps] preds]
    (cond (nil? p) true
          (p x) (recur ps)
          :else false)))

(defn- deriv [p [{:keys [idx form] :as c0} & cs :as context] prefix]
  (let [{ps :ps :keys [::s/op p1] :as p} (reg-resolve! p)]
    (when p
      (case op
        ::s/accept nil
        nil (let [candidates (candidates* p cs prefix)]
              (when (set? candidates)
                candidates))
        ::s/amp (let [candidates (deriv p1 context prefix)]
                  (into #{} (filter #(and-all ps %) candidates)))
        ::s/pcat (deriv (get ps idx) context prefix)
        ::s/alt (apply clojure.set/union (doall (map #(deriv % context prefix) ps)))
        ::s/rep (deriv p1 context prefix)))))

(defn regex-spec-impl [re]
  (reify
    Specize
    (specize* [s] s)
    (specize* [s _] s)
    Complete
    (candidates* [_ [{:keys [idx form]} & _ :as context] prefix]
      (if (coll? form)
        (deriv re context prefix)
        nil))))

(defn spec-impl
  ([pred]
   (cond
     #_(spec? pred) #_(cond-> pred gfn (with-gen gfn))
     (s/regex? pred) (regex-spec-impl pred)
     #_(ident? pred) #_(cond-> (the-spec pred) gfn (with-gen gfn))
     :else
     (reify
       Specize
       (specize* [s] s)
       (specize* [s _] s)
       
       Complete
       (candidates* [_ context prefix]
         (let [ret (candidates* pred context prefix)]
           (when (set? ret) ret)))))))

(extend-protocol Specize
  clojure.lang.Keyword
  (specize* ([k] (specize* (reg-resolve! k)))
            ([k _] (specize* (reg-resolve! k))))

  clojure.lang.Symbol
  (specize* ([s] (specize* (reg-resolve! s)))
            ([s _] (specize* (reg-resolve! s))))

  Object
  (specize*
    ([o] (spec-impl o))
    ([o form] (spec-impl o))))

(defn- specize
  ([s] (c/or (complete? s) (specize* s)))
  ([s form] (c/or (complete? s) (specize* s form))))

(defn candidates [spec context prefix]
  (candidates* (specize spec) context prefix))





(comment
  (require '[ewen.replique.replique-conf :as conf])

  (candidates "" '({:idx nil, :map-role :key, :form {__prefix__ nil}}) "eee")
  
  
  )

(comment

  ;; Regexps
  (s/def ::rr string?)
  (s/def ::ss #{11111 222222})
  
  (candidates (-> (s/cat :e (s/cat :f #{1111 2}))
                  s/form
                  form->spec)
              '({:idx 0, :form [__prefix__ 33]})
              "11")

  (candidates (-> (s/cat :a (s/alt :b #{1111 2} :c #{3333 4444}) :d #{"eeeeee"})
                  s/form
                  form->spec)
              '({:idx 1, :form [nil __prefix__]})
              "eee")

  (candidates (-> (s/cat :a ::ss)
                  s/form
                  form->spec)
              '({:idx 0, :form [__prefix__]})
              "111")

  (candidates (-> (s/cat :a (s/spec #{11111}))
                  s/form
                  form->spec)
              '({:idx 0, :form [__prefix__]})
              "111")

  (candidates (-> (s/* (s/cat :a (s/alt :b string? :c #{1111})))
                  s/form
                  form->spec)
              '({:idx 0, :form [__prefix__ 33]})
              "11")

  (candidates (-> (s/* #{1111})
                  s/form
                  form->spec)
              '({:idx 0, :form [[__prefix__]]} {:idx 0, :form [__prefix__]})
              "11")

  (candidates (-> (s/& (s/cat :e #{11111 "eeeeeeee"}) string? string?)
                  s/form
                  form->spec)
              '({:idx 0, :form [__prefix__]})
              "eee")

  (form->spec (s/form (s/cat :e (fn [] nil))))
  
  )

