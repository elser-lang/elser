(ns evmlisp.types
  (:gen-class)
  (:require [evmlisp.errors :as errs]))

(defrecord Type [name primitive? size])

(defn array-size [arr]
  (* 32 (get (second arr) 0)))
(def type-table
  {:i256  (->Type "int256" true 32)
   :bool  (->Type "bool" true 1)
   :addr  (->Type "address" true 20)
   :b32  (->Type "bytes32" true 32)
   'map   (->Type "mapping" false 32) ; The only dynamic type.
   'array (fn [arr] 
            (->Type "array" false (array-size arr)))
   })

;; TODO: explain.
(defn common-type-form [t]
  (println "t" t)
  (if (list? t)
    (let [f (first t)]
      (println "f?" f)
      (cond
        (= f 'array)
        ':i256

        (= f 'map)
        (let [m [(list (second t)) (common-type-form (last t))]]
          ;; Return map without the last return.
          (subvec m 0 (- (count m) 1)))

        :else
        (errs/err-bad-type t "map | array")))
    
    (list t)))


(defn valid-type? [t]
  ;; If list validate it as complex type.
  (if (list? t)
    (if (contains? type-table (first t))
      (let [f (first t)
            l (last t)]
        
      (cond
        (= f 'array)        
        (do (valid-type? l)
            ((get type-table f) t)) ; Execute array fn.

         (= f 'map)
         (do
           (valid-type? l)
           (get type-table f))

         :else
         (errs/err-bad-type t "map | array")))
        
        (errs/err-bad-type t "1)"))
    
    (if (contains? type-table t)
      (get type-table t)
      (errs/err-bad-type t "2)"))))

(defmulti infer-type (fn [form symtab] (:type form)))

(defn ensure-numeric [args symtab]
  (doseq [arg args]
    (let [t (infer-type arg symtab)]
      (when-not (contains? #{:i256} t)
        (errs/err-bad-type "non-numeric" t)))))

;; TODO: extend with type checks.
(defmethod infer-type :call [[op args] symtab]
  (cond
    (or (= op '+) (= op '-))
    (ensure-numeric args symtab)

    :else
    "xyi"))
