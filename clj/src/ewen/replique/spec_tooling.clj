(ns ewen.replique.spec-tooling
  (:refer-clojure :exclude [+ * and assert or cat def keys merge])
  (:require [clojure.spec :as s]
            [ewen.replique.replique-conf :as conf]
            [clojure.pprint :refer [pprint pp]]
            [clojure.walk :as walk]))

(alias 'c 'clojure.core)

#_(defn spec-keys-candidates [^String prefix form spec]
  (let [[spec-sym & spec-rest :as spec-data] (s/form spec)]
    (cond (= 'clojure.spec/multi-spec spec-sym)
          (let [mm (-> spec-data second resolve)
                dispatch-vals (-> spec-data second keyword s/form)]
            nil)
          (= 'clojure.spec/or spec-sym)
          (->> (drop 1 spec-rest)
               (take-nth 2)
               (map spec-keys-candidates)
               (apply clojure.set/union))
          (= 'clojure.spec/and spec-sym)
          (->> (drop 1 spec-rest)
               (take-nth 2)
               (map spec-keys-candidates)
               (apply clojure.set/intersection))
          (= 'clojure.spec/every spec-sym)
          (let [[maybe-tuple tuple-keys tuple-vals] (second spec-data)
                every-args (apply hash-map (drop 2 spec-data))]
            (when (and (= 'clojure.spec/tuple maybe-tuple)
                       (set? tuple-keys)
                       (= 'clojure.core/map? (get every-args :s/kind-form)))
              tuple-keys))
          (= 'clojure.spec/keys spec-sym)
          (let [keys-args (apply hash-map spec-rest)]
            (->> (concat (:req keys-args) (:req-un keys-args)
                         (:opt keys-args) (:opt-un keys-args))
                 (into #{})))
          (= 'clojure.spec/merge spec-sym)
          (->> (mapcat spec-keys-candidates spec-rest)
               (into #{}))
          :else nil)))

#_(defn candidates [^String prefix context spec]
  (let [{:keys [map-role form]} (first context)]
    (cond (= :key map-role)
          nil
          :else nil))
  )

(declare candidates)

(defprotocol Spec
  (candidates* [spec form]))

(defn spec? [x]
  (when (instance? ewen.replique.spec_tooling.Spec x) x))

(declare form->spec)

(defn cat-form->spec [key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pf (mapv second pairs)
        pred-forms (mapv form->spec pf)]
    (s/cat-impl keys pred-forms pf)))

(defn alt-form->spec [key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pf (mapv second pairs)
        pred-forms (mapv form->spec pf)]
    (s/alt-impl keys pred-forms pf)))

(defn amp-form->spec [re preds]
  (let [pf (vec preds)]
    (s/amp-impl re (mapv form->spec pf) pf)))

(defn rep-form->spec [pred-form]
  (s/rep-impl pred-form (form->spec pred-form)))

(declare spec-impl)
(declare map-spec-impl)

(defn keys-form->spec [{:keys [req req-un opt opt-un]}]
  (let [unk #(-> % name keyword)
        req-keys (filterv keyword? (flatten req))
        req-un-specs (filterv keyword? (flatten req-un))
        req-specs (into req-keys req-un-specs)
        req-keys (into req-keys (map unk req-un-specs))
        opt-keys (into (vec opt) (map unk opt-un))
        opt-specs (into (vec opt) opt-un)
        parse-req (fn [rk f]
                    (map (fn [x]
                           (if (keyword? x)
                             #(contains? % (f x))
                             (walk/postwalk
                               (fn [y] (if (keyword? y) #(contains? % (f y)) y))
                               x)))
                         rk))
        pred-exprs [map?]
        pred-exprs (into pred-exprs (parse-req req identity))
        pred-exprs (into pred-exprs (parse-req req-un unk))
        keys-pred #(loop [[pred-expr & pred-exprs-rest] pred-exprs]
                     (cond
                       (nil? pred-expr) true
                       (pred-expr %) (recur pred-exprs-rest)
                       :else false))]
    (map-spec-impl {:req req :opt opt :req-un req-un :opt-un opt-un
                    :req-keys req-keys :req-specs req-specs
                    :opt-keys opt-keys :opt-specs opt-specs
                    :pred-exprs (vec pred-exprs)
                    :keys-pred keys-pred})))

(defn form->spec [form]
  (cond (symbol? form)
        (spec-impl @(resolve form))
        (keyword? form)
        form
        (ifn? form)
        (spec-impl form)
        :else
        (let [[spec-sym & spec-rest] form]
          (cond (= 'clojure.spec/cat spec-sym)
                (cat-form->spec spec-rest)
                (= 'clojure.spec/alt spec-sym)
                (alt-form->spec spec-rest)
                (= 'clojure.spec/& spec-sym)
                (amp-form->spec (form->spec (first spec-rest)) (drop 1 spec-rest))
                (= 'clojure.spec/* spec-sym)
                (rep-form->spec (first spec-rest))

                (= 'clojure.spec/keys spec-sym)
                (keys-form->spec (apply hash-map spec-rest))))))

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

(defn- maybe-spec
  "spec-or-k must be a spec, regex or resolvable kw/sym, else returns nil."
  [spec-or-k]
  (let [s (c/or (c/and (ident? spec-or-k) (reg-resolve spec-or-k))
                (spec? spec-or-k)
                (s/regex? spec-or-k)
                nil)]
    (if (s/regex? s)
      (#'s/with-name (regex-spec-impl s) (#'s/spec-name s))
      s)))

(defn- the-spec
  "spec-or-k must be a spec, regex or kw/sym, else returns nil. Throws if unresolvable kw/sym"
  [spec-or-k]
  (c/or (maybe-spec spec-or-k)
        (when (ident? spec-or-k)
          (throw (Exception. (str "Unable to resolve spec: " spec-or-k))))))

(defn- dt
  ([pred x form] (dt pred x form nil))
  ([pred x form cpred?]
     (if pred
       (if-let [spec (the-spec pred)]
         (candidates spec x)
         (if (ifn? pred)
           (if cpred?
             (pred x)
             (if (pred x) x ::s/invalid))
           (throw (Exception. (str (pr-str form) " is not a fn, expected predicate fn")))))
       x)))

#_(defn- alt* [ps ks forms]
  (prn "alt* " (#'s/filter-alt ps ks forms identity))
  (let [[[p1 & pr :as ps] [k1 :as ks] forms] (#'s/filter-alt ps ks forms identity)]
    (when ps
      (let [ret {::s/op ::s/alt, :ps ps, :ks ks :forms forms}]
        (if (nil? pr) 
          (if k1
            (if (#'s/accept? p1)
              (#'s/accept (#'s/tagged-ret k1 (:ret p1)))
              ret)
            p1)
          ret)))))

(defn- deriv [p x]
  (let [{[p0 & pr :as ps] :ps, [k0 & kr :as ks] :ks, :keys [::s/op p1 p2 ret splice forms] :as p}
        (reg-resolve! p)]
    (prn "--- deriv --")
    (prn "p: " p)
    (prn "x: " x)
    (when p
      (case op
        ::s/accept nil
        nil (let [ret (dt p x p)]
              (when-not (s/invalid? ret) (#'s/accept ret)))
        ::s/amp (when-let [p1 (deriv p1 x)]
                  (if (= ::s/accept (::s/op p1))
                    (let [ret (-> (#'s/preturn p1) (#'s/and-preds ps (next forms)))]
                      (when-not (s/invalid? ret)
                        (#'s/accept ret)))
                    (s/amp-impl p1 ps forms)))
        ::s/pcat (#'s/alt2 (#'s/pcat*
                            {:ps (cons (deriv p0 x) pr), :ks ks, :forms forms, :ret ret})
                           (when (#'s/accept-nil? p0)
                             (deriv
                              (#'s/pcat*
                               {:ps pr, :ks kr, :forms (next forms),
                                :ret (#'s/add-ret p0 ret k0)}) x)))
        ::s/alt (#'s/alt* (doall (map #(deriv % x) ps)) ks forms)
        ::s/rep (#'s/alt2 (#'s/rep* (deriv p1 x) p2 ret splice forms)
                          (when (#'s/accept-nil? p1)
                            (deriv
                             (#'s/rep* p2 p2 (#'s/add-ret p1 ret nil) splice forms)
                             x)))))))

(defn- re-candidates [p [x & xs :as data]]
  ;;(prn {:p p :x x :xs xs})
  (if (empty? data)
    (if (#'s/accept-nil? p)
      (let [ret (#'s/preturn p)]
        (if (= ret ::s/nil)
          nil
          ret))
      ::s/invalid)
    (let [dp (deriv p x)]
      (if dp
        (do
          (prn "-----deriv-ret-----")
          (prn dp)
          (recur dp xs))
        ::s/invalid))))

(defn regex-spec-impl [re]
  (reify
    Specize
    (specize* [s] s)
    (specize* [s _] s)

    Spec
    (candidates* [_ x]
      (if (c/or (nil? x) (coll? x))
        (re-candidates re (seq x))
        ::s/invalid))))

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
         
         Spec
       (candidates* [_ x] (let [ret (pred x)]
                            (if ret x ::s/invalid)))))))

(defn map-spec-impl
  [{:keys [req-un opt-un keys-pred pred-exprs opt-keys req-specs req req-keys opt-specs opt]
    :as argm}]
  (let [k->s (zipmap (concat req-keys opt-keys) (concat req-specs opt-specs))
        keys->specnames #(c/or (k->s %) %)
        id (java.util.UUID/randomUUID)]
    (reify
      Specize
      (specize* [s] s)
      (specize* [s _] s)
      Spec
      (candidates* [_ m]
        (if (keys-pred m)
          (let [reg (s/registry)]
            (loop [ret m, [[k v] & ks :as keys] m]
              (if keys
                (let [sname (keys->specnames k)]
                  (if-let [s (get reg sname)]
                    (let [s (form->spec (s/form s))
                          cv (candidates s v)]
                      (if (s/invalid? cv)
                        ::s/invalid
                        (recur (if (identical? cv v) ret (assoc ret k cv))
                               ks)))
                    (recur ret ks)))
                ret)))
          ::s/invalid)))))

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
  ([s] (c/or (spec? s) (specize* s)))
  ([s form] (c/or (spec? s) (specize* s form))))

(defn candidates [spec x]
  (candidates* (specize spec) x))





(comment
  (require '[ewen.replique.replique-conf :as conf])

  (candidates "" '({:idx nil, :map-role :key, :form {__prefix__ nil}}) ::conf/cljs-env)
  (s/form ::conf/cljs-env)
  (s/form ::conf/cljs-env-type)
  
  ((get-method ewen.replique.replique-conf/cljs-env-type :browser) nil)

  (ewen.replique.replique-conf/cljs-env-type {:cljs-env-type :browser})

  (s/conform (s/and (s/keys :req-un [::cljs])
                    #(contains? (:cljs %) :main)) {:cljs {:main nil}})
  
  )

(comment

  ;; Regexps
  (s/def ::rr string?)
  
  (candidates (-> (s/cat :e ::rr)
                  s/form
                  form->spec)
              ["e"])
  (s/conform (s/cat :e ::rr) ["e"])


  (candidates (-> (s/alt :e string? :f string?)
                  s/form
                  form->spec)
              ["e"])
  (s/conform (s/alt :e string? :f string?) ["e"])

  (candidates (-> (s/& (s/cat :e string?) coll?)
                  s/form
                  form->spec)
              ["e"])
  (s/conform (s/& (s/cat :e string?) coll?) ["e"])

  (form->spec (s/form (s/* string?)))
  (candidates (-> (s/* string?)
                  s/form
                  form->spec)
              [3])
  (s/conform (s/* string?) [3])

  ;; keys

  (s/def ::kk string?)

  (candidates (form->spec (s/form (s/keys :req [::kk]))) {::kk 3})
  (s/conform (s/keys :req [::kk]) {::kk 3})
  )
