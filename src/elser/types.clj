(ns elser.types
  (:gen-class)
  (:require [elser.errors :as errs]))

(def ADDR_REGEXP #"^0x[a-fA-F0-9]{40}$")

(def elser-types
  {:i256 ':i256
   :u256 ':u256
   :bool ':bool 
   :addr ':addr
   :b32 ':b32
   })

(def numeric [:u256 :i256])
(def boolean [:bool])
(def addr [:addr])
(def all [:u256 :i256 :bool :addr :b32])

;; TODO: handle :b32 type.
(defn infer-type [x]
  (cond
    (or (pos-int? x) (= 0 x))
    :u256

    (int? x)
    :i256    

    (boolean? x)
    :bool

    (re-matches ADDR_REGEXP (str x))
    :addr

    (map? x)
    (cond
      (get x :return)
      (infer-type (first (get x :return)))
      
      (get x :type)
      (get x :type)
      
      :else
      (errs/err-unsupported-literal-type x))

                                        ;(symbol? x)
                                        ;(if (get elser-types x)
                                        ; x
                                        ;(errs/err-unsupported-literal-type x))
    
    :else
    (errs/err-unsupported-literal-type x)))

(defn type-check
  "Check if type definition is valid Elser type."
  [x]
  (if (:type x) ; If type is already defined check if it's valid.
    (if (get elser-types (:type x))
      x
      (errs/err-bad-type (:type x)))
    {:type (infer-type x)}))

(defn type-check-op
  "Require that every type(...t) = t from T"
  [T t]
  (if (empty? t) (throw (Exception. "type-check-op: empty (t)")))
  (reduce (fn [x y]
            (do
              (if (not (some #{(:type y)} x))
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
  (let [x-type (type-check x)
        y-type (type-check y)]
    (do (type-check-op numeric [x-type y-type])
        (if (nil? ret-t)
          x-type
          ret-t))))

(defn type-check-numeric-unary
  "
  Checks types for a binary numeric operation
  and returns required return type.
  "
  [x]
  (let [x-type (type-check x)]
    (do (type-check-op numeric [x-type])
        x-type)))

(defn type-check-assign
  "
  Checks types for assignement operation
  and returns type of 'expr'.
  "
  [bind expr]
  (if (:mutable? bind)
    (let [bind-type (type-check bind)
          expr-type (type-check expr)]
      (do (type-check-op [(:type bind-type)] [expr-type])
          expr-type))
    (errs/err-set-on-immutable bind)))

(defn type-check-bool
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [x y ret-t]
  (do (type-check-op boolean [(type-check x) (type-check y)])
      ret-t))

(defn type-check-all
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [x y ret-t]
  (let [x-type (type-check x)
        y-type (type-check y)]
    (if (not (= (:type x-type) (:type y-type)))
      (errs/err-diff-types-comp x-type y-type))
    (do (type-check-op all [x-type y-type])
        ret-t)))

(defn type-check-all-unary
  "
  Checks types for a binary comparison operation
  and returns required return type.
  "
  [x ret-t]
  (do (type-check-op all [(type-check x)])
      ret-t))

(defn type-check-binary-multi [x y T ret-t]
  (do (type-check-op T [(type-check x) (type-check y)])
      ret-t))

(defn type-check-bool-or-num
  [x y ret-t]
  (do (type-check-op (into boolean numeric)  [(type-check x) (type-check y)])
      ret-t))

(defn type-check-bool-unary
  "
  Checks types for an unary comparison operation
  and returns required return type.
  "
  [x ret-t]
  (do (type-check-op boolean [(type-check x)])
      ret-t))
