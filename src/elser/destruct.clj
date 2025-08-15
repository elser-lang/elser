(ns elser.destruct
  (:gen-class)
  (:require [elser.errors :as errs]))

(def RETURN_SYMBOL '->)
(def DEF_SYMBOL 'def)
(def DEFN_SYMBOL 'defn)

(def constant-form 'const)
(def base-type 'base)
(def map-type 'map)
(def list-type 'list)

(defrecord def-form [name args ret opts type])
(defrecord defn-form [name args access ret body])

(declare form-type)

(defn symbol-type [s args]
  (cond
    (list? s) (form-type s args)
    :else s))

(defn form-type
  "Check type of the form and return its args and ret values."
  [types args]
  (let [var-type (first types)]
    (cond
      (= var-type 'map)
      (conj args
            (symbol-type (nth types 1) args) ; arg
            (symbol-type (nth types 2) args) ; ret
            )

      :else
      var-type
      )
    ))

(defn extract-args-and-ret
  "Given possibly nested vector of types extract arguments and return types."
  [types]
  (loop [i 0
         args []
         t types]
    (if (not (vector? t)) {:args args :ret t}

        (recur
         (inc i)
         (conj args {:name (str "var_" i)
                     :type (first t)
                     :mutable? nil})
         (second t))
        )
    ))

(defn extract-def-type 
  "Return type of a definition: (base | map | list)"
  [first-type]
  (cond

    (= first-type 'map) map-type
    (= first-type 'list) list-type
    :else base-type

    ))

(defn destruct-def
  "Convert form list into def-form."
  [form]  
  (let [[key name types & opts] form]
    (if (not (= key DEF_SYMBOL))
      (errs/err-invalid-def-key key 'def name))
    (if (not (= (first types) RETURN_SYMBOL))
      (errs/err-incorrect-return-symbol (first types)))

    (let [type-list (second types)
          result (form-type type-list [])
          args-ret (extract-args-and-ret result)]
      (def-form.
        name
        (:args args-ret)
        (list {:name "_elser_ret_val" :type (:ret args-ret)})
        (first opts)
        (extract-def-type 
         (first type-list)))
      )
    ))
