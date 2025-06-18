(ns elser.types
  (:gen-class)
  (:require [elser.errors :as errs]))

(def elser-types
  {;:i256 ':i256
   :u256 ':u256
   :bool ':bool 
   :addr ':addr
   :b32 ':b32
   })

(def numeric [:u256]) ;; :i256
(def boolean [:bool])
(def addr [:addr])
(def all [:u256 :bool :addr :b32]) ; :i256

(defn infer-type [x]
  (let [t (type x)]
    (cond
      (= t java.lang.Integer)
      :u256

      (= t java.lang.Boolean)
      :bool

      (= t java.lang.String) ; fix: this is quite wrong
      :addr

      (= t clojure.lang.PersistentArrayMap)
      (cond
        (get x :return)
        (infer-type (first (get x :return)))

        (get x :type)
        (get x :type)

        :else
        (errs/err-unsupported-literal-type x))

      (= t clojure.lang.Keyword)
      (if (get elser-types x)
        x
        (errs/err-unsupported-literal-type x))
          
      :else
      (errs/err-unsupported-literal-type x))))

(defn type-check
  "Check if type definition is valid Elser type."
  [x]
  (if (:type x)
    (if (get elser-types (:type x)) (:type x) (errs/err-bad-type (:type x)))
    (infer-type x)))

(defn type-check-op
  "Require that every type(...t) = t from T"
  [T t]
  (reduce (fn [x y]
            (do
              (if (not (some #{y} x))
                (errs/err-bad-type-op T t)
                x)))
          T t))

(defn type-check-addr
  [x]
  (do (type-check-op addr [(type-check x)])
      (:type x)))

(defn type-check-numeric
  "
  Checks types for a binary numeric operation
  and returns required return type.
  "
  [x y ret-t]
  (do (type-check-op numeric [(type-check x) (type-check y)])
      {:type ret-t}))

(defn type-check-numeric-unary
  "
  Checks types for a binary numeric operation
  and returns required return type.
  "
  [x]
  (do (type-check-op numeric [(type-check x)])
      {:type x}))

(defn type-check-assign
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [bind expr]
  (if (:mutable? (:type bind))
    (do (type-check-op [(:type (:type bind))] [(type-check expr)])
        bind)
        (errs/err-set-on-immutable bind)))

(defn type-check-bool
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [x y ret-t]
  (do (type-check-op boolean [(type-check x) (type-check y)])
      {:type ret-t}))

(defn type-check-all
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [x y ret-t]
  (do (type-check-op all [(type-check x) (type-check y)])
      {:type ret-t}))

(defn type-check-all-unary
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [x ret-t]
  (do (type-check-op all [(type-check x)])
      {:type ret-t}))

(defn type-check-binary-multi [x y T ret-t]
  (do (type-check-op T [(type-check x) (type-check y)])
      {:type ret-t}))

(defn type-check-bool-or-num
  [x y ret-t]
  (do (type-check-op (into boolean numeric)  [(type-check x) (type-check y)])
      {:type ret-t}))

(defn type-check-bool-unary
  "
  Checks types for an unary comparison operation
  and returns required return type.
  "
  [x ret-t]
  (do (type-check-op boolean [(type-check x)])
      {:type ret-t}))
