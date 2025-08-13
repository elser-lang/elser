(ns elser.destruct
  (:gen-class)
  (:require [elser.errors :as errs]))

(def RETURN_SYMBOL '->)
(def DEF_SYMBOL 'def)
(def DEFN_SYMBOL 'defn)

(def constant-form 'const)
(def storage-base-type 'sto-base)
(def storage-map 'sto-map)
(def storage-list 'sto-list)

(defrecord def-form [name args ret opts])
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
  "Given possibly nested vector of types extracts arguments and return types."
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

(defn destruct-def
  "Convert form list into def-form."
  [form]
  (let [[key name types & opts] form]
    (if (not (= key DEF_SYMBOL))
      (errs/err-invalid-def-key key 'def name))
    (if (not (= (first types) RETURN_SYMBOL))
      (errs/err-incorrect-return-symbol (first types)))

    (let [result (form-type (second types) [])
          args-ret (extract-args-and-ret result)]
      (println "result" result)
      (println "args-ret" args-ret)
      (def-form.
        name
        (:args args-ret)
        (list {:name "_elser_ret_val" :type (:ret args-ret)})
        (first opts))
      )
    ))
