(ns ewen.replique.spec-tooling
  (:refer-clojure :exclude [+ * and assert or cat def keys merge])
  (:require [clojure.spec :as s]
            [ewen.replique.replique-conf :as conf]
            [clojure.pprint :refer [pprint pp]]
            [clojure.walk :as walk]))

(alias 'c 'clojure.core)

(declare candidates)
(declare from-spec)
(declare map-spec-impl)
(declare multi-spec-impl)

(defprotocol Complete
  (candidates* [spec context prefix]))

(extend-protocol Complete
  clojure.lang.Var
  (candidates* [v context prefix]
    @v))

(extend-protocol Complete
  clojure.spec.Spec
  (candidates* [spec context prefix]
    (candidates (from-spec spec) context prefix)))

(extend-protocol Complete
  clojure.lang.Keyword
  (candidates* [k context prefix]
    (when-let [spec (s/get-spec k)]
      (candidates (from-spec spec) context prefix))))

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

(defn complete? [x]
  (when (instance? ewen.replique.spec_tooling.Complete x) x))

(defn from-cat [preds]
  {::s/op ::s/pcat :ps (mapv from-spec preds)})

(defn from-alt [preds]
  {::s/op ::s/alt :ps (mapv from-spec preds)})

(defn from-rep [pred]
  {::s/op ::s/alt :p1 (from-spec pred)})

(defn from-amp [pred preds]
  {::s/op ::s/alt :p1 (from-spec pred) :ps (mapv from-spec preds)})

(defn from-keys [keys-seq]
  (-> (apply hash-map keys-seq)
      map-spec-impl))

(defn from-multi [mm]
  (multi-spec-impl (resolve mm)))

(defn from-every [pred]
  )

;; Form is a spec OR form is a qualified symbol and can be resolved to a var OR form is something
;; that get evaled in order to get the values than the ones from core.spec spec impls.
;; Qualified symbols are not evaled in order to customize the candidates* behavior for vars
;; It is resolved to a var and not kept as a symbol, otherwise reg-resolve would try to resolve
;; it to a spec
(defn from-spec [spec]
  (when spec
    (cond
      (s/regex? spec) (case (::s/op spec)
                        ::s/pcat (from-cat (:forms spec))
                        ::s/alt (from-alt (:forms spec))
                        ::s/rep (from-rep (:forms spec))
                        ::s/amp (from-amp (:p1 spec) (:forms spec)))
      (s/spec? spec)
      (let [form (s/form spec)
            [spec-sym & spec-rest] form]
        (cond (= 'clojure.spec/keys spec-sym)
              (from-keys spec-rest)
              (= 'clojure.spec/multi-spec spec-sym)
              (from-multi (first spec-rest))
              (= 'clojure.spec/every spec-sym)
              (from-every (first spec-rest))
              :else (eval form)))
      (c/and (symbol? spec) (namespace spec) (var? (resolve spec)))
      (resolve spec)
      :else (eval spec))))

(defprotocol Specize
  (specize* [_] [_ form]))

(declare regex-spec-impl)

(defn- reg-resolve [k]
  (if (ident? k)
    (let [reg @@#'s/registry-ref
          spec (get reg k)]
      (when spec
        (if-not (ident? spec)
          (from-spec spec)
          (-> (#'s/deep-resolve reg spec) from-spec))))
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
    Complete
    (candidates* [_ [{:keys [idx form]} & _ :as context] prefix]
      (when (coll? form)
        (deriv re context prefix)))))

(defn spec-impl [pred]
  (cond
    (s/regex? pred) (regex-spec-impl pred)
    :else
    (reify
      Specize
      (specize* [s] s)
      Complete
      (candidates* [_ context prefix]
        (let [ret (candidates* pred context prefix)]
          (when (set? ret) ret))))))

(defn map-spec-impl [{:keys [req-un opt-un req opt]}]
  (let [req-un->req (zipmap (map (comp keyword name) req-un) req-un)
        opt-un->opt (zipmap (map (comp keyword name) opt-un) opt-un)
        unqualified->qualified (c/merge req-un->req opt-un->opt)]
    (reify
      Specize
      (specize* [s] s)
      Complete
      (candidates* [_ [{:keys [idx map-role form]} & cs :as context] prefix]
        (when (map? form)
          (case map-role
            :value (when-let [spec (s/get-spec (c/or (get unqualified->qualified idx) idx))]
                     (candidates spec cs prefix))
            :key (candidates* (into #{} (concat (c/keys unqualified->qualified) req opt))
                              nil prefix)))))))

(defn multi-spec-impl [mm]
  (reify
    Specize
    (specize* [s] s)
    Complete
    (candidates* [_ [{:keys [form]} & cs :as context] prefix]
      (let [spec (try (mm form)
                      (catch IllegalArgumentException e nil))
            specs (if spec #{spec} (->> (vals (methods @mm))
                                        (map (fn [spec-fn] (spec-fn nil)) )
                                        (into #{})))]
        (->> (map #(candidates % context prefix) specs)
             (apply clojure.set/union))))))

(defn every-impl []
  )

(extend-protocol Specize
  clojure.lang.Keyword
  (specize* [k] (specize* (reg-resolve! k)))

  clojure.lang.Symbol
  (specize* [s] (specize* (reg-resolve! s)))

  clojure.lang.IFn
  (specize* [ifn] (spec-impl ifn))

  clojure.spec.Spec
  (specize* [spec] (from-spec spec)))

(defn candidates [spec context prefix]
  (when (satisfies? Specize spec)
    (candidates* (specize* spec) context prefix)))





(comment
  (require '[ewen.replique.replique-conf :as conf])

  (candidates "" '({:idx nil, :map-role :key, :form {__prefix__ nil}}) "eee")
  
  )

(comment

  ;; Regexps
  (s/def ::rr string?)
  (s/def ::ss #{11111 222222})
  
  (candidates (s/cat :e (s/cat :f #{1111 2}))
              '({:idx 0, :form [__prefix__ 33]})
              "11")

  (candidates (s/cat :a (s/alt :b #{1111 2} :c #{3333 4444}) :d #{"eeeeee"})
              '({:idx 1, :form [nil __prefix__]})
              "eee")

  (candidates (s/cat :a ::ss)
              '({:idx 0, :form [__prefix__]})
              "111")

  (candidates (s/cat :a (s/spec #{11111}))
              '({:idx 2, :form [nil nil __prefix__]})
              "111")

  (candidates (s/* (s/cat :a (s/alt :b string? :c #{1111})))
              '({:idx 0, :form [__prefix__ 33]})
              "11")

  (candidates (s/* #{1111})
              '({:idx 0, :form [[__prefix__]]} {:idx 0, :form [__prefix__]})
              "11")

  (candidates (s/& (s/cat :e #{11111 "eeeeeeee"}) string? string?)
              '({:idx 0, :form [__prefix__]})
              "eeee")



  
  (candidates (s/keys :req [::ss])
              '({:idx ::ss :map-role :value :form {::ss __prefix__}})
              "11")

  (candidates (s/keys :req [::ss])
              '({:idx nil :map-role :key :form {__prefix__ nil}})
              ":ewen")

  (candidates (s/keys :req-un [::ss])
              '({:idx nil :map-role :key :form {__prefix__ nil}})
              ":s")



  ;; multi spec

  #_(candidates ::conf/cljs-env
              '({:idx nil :map-role :key :form {__prefix__ nil}})
              ":ewen")

  (defmulti test-mm :mm)
  (defmethod test-mm 33 [_]
    (s/keys :req-un [::ss]))
  (candidates (s/multi-spec test-mm :mm)
              '({:idx nil :map-role :key :form {:ss 33
                                                __prefix__ nil}})
              ":s")
  (candidates (s/multi-spec test-mm :ss)
              '({:idx :ss :map-role :value :form {:mm 33
                                                  :ss __prefix__}})
              "11")


  ;; every

  (s/form (s/& string? number?))
  )
