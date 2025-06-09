(ns evmlisp.symtable
  (:gen-class)
  (:import [org.web3j.crypto Hash]
           [org.web3j.utils Numeric])
  (:require [clojure.string :as string]
            [evmlisp.errors :as errs]
            [evmlisp.core :as core]))

(defn obtain-selector
  "Converts a Solidity function signature to a 4-byte selector"
  [signature]
  (let [hash-bytes (Hash/sha3 (.getBytes signature))
        selector-bytes (byte-array 4)]
    (System/arraycopy hash-bytes 0 selector-bytes 0 4)
    (Numeric/toHexString selector-bytes)))

(defn defn-to-signature
  "Converts evmlisps's definitions to function signatures."
  [fn-name args]
  (format "%s(%s)" fn-name
          ;; Get all types of a function defintion.
          (string/join ","
                       (map name
                            (map (fn [v] (get (vec v) 1)) args)))))

;; TODO: it doesn't account for mappings.
(defn def-to-signature
  "Converts evmlisps's external storage definitions to function signatures."
  [def-name sto-types]
    (if (some #{'=>} sto-types) ; if type is a mapping.
      (let [new-types (remove #{'=>} sto-types)
            len (count new-types)]
        (recur def-name
               (subvec (vec new-types) 0 (- len 1))))
        (format "%s(%s)" def-name
                (string/join ","
                             (map name sto-types)))))

(defn args-to-symbols
  "
  Produces {:name ... :type ...} map on
  a given [(arg_0 :type) ... (arg_n :type)]
  "
  [args]
  (map-indexed (fn [i v]
         (let [named? (= (mod (count v) 2) 0)
               arg-name (if named?
                          (get (vec v) 0)
                          (str "arg_" i))
               arg-type (if named?
                          (get (vec v) 1)
                          (get v 0))]
           {:name arg-name
            :type arg-type}))
         args))

(def supported-function-types ['defn 'defn-read])
(defn function-type
  [definition]
  (if (some #{definition} supported-function-types)
    definition
    (errs/err-unsupported-function-def definition supported-function-types)))

(defn process-ex-in
  [objects]
  (let [x (first (rest objects))
        external (:external x)
        internal (:internal x)]
    {:external external
     :internal internal}))

;; TODO: implement
(defn process-constructor [constructor] constructor)

(defn process-functions [functions]
  (let [definitions (process-ex-in functions)]
      (let [initial-state {:functions
                       {:external [] :internal []}}]
    (reduce (fn [state [visibility defs]]
              (reduce (fn [state def-form]
                        (let [[fn-type fn-name args ret body] def-form
                              sig (defn-to-signature fn-name args)
                              var-def {:name fn-name
                                       :selector (obtain-selector sig)
                                       :signature sig
                                       :args (args-to-symbols args)
                                       :fn-type (function-type fn-type)
                                       :body body
                                       :return (map name (vec ret))}]
                          (-> state
                              (update-in [:functions visibility] conj var-def))))
                      state
                      defs))
            initial-state
            [[:external (:external definitions)]
             [:internal (:internal definitions)]]))))

;; FIX: this function:
;; - can't increment storage counter for data that occupies more than 32 bytes.
;; - looks ugly...
(defn process-storage [storage]
  (let [definitions (process-ex-in storage)
        initial-state {:slot-counter 0x00
                       :storage {:external [] :internal []}
                       :occupied-slots []}]
    (reduce (fn [state [visibility defs]]
              (reduce (fn [state def-form]
                        (let [[_ def-name sto-types & opts] def-form
                              ;; Use custom slot if specified, otherwise allocate new.
                              custom-slot (when (map? (last opts)) (:slot (last opts)))
                              slot (or custom-slot (:slot-counter state))
                              sig (def-to-signature def-name sto-types)
                              ;; Increment counter if using auto-allocation.
                              new-counter (if custom-slot
                                            (:slot-counter state)
                                            (inc slot))
                              var-def {:name def-name
                                       :selector (obtain-selector sig)
                                       :signature sig
                                       :args (args-to-symbols (list sto-types))
                                       :slot slot
                                       :return (map name (subvec sto-types
                                                       (- (count sto-types) 1)))}]
                          ;; Check for storage collision.
                          (if (some #{slot} (:occupied-slots state))
                            (errs/err-slot-collision slot)
                            (-> state
                                (update-in [:storage visibility] conj var-def)
                                (assoc :slot-counter new-counter)
                                (update-in [:occupied-slots] conj slot)))
                          ))
                      state
                      defs))
            initial-state
            [[:external (:external definitions)]
             [:internal (:internal definitions)]])))

(defn collect-symbols
  "Produces a symbol table on a given AST."
  [ast]
  (reduce
   (fn [symbols form]
     (cond
       ;; Parses all the outer-layer constructs.
       
       ;; Namespace defintion.
       (and (list? form) (= 'ns (first form)))
       (assoc symbols :ns (second form))

       (and (list? form) (= 'constructor (first form)))
       (merge symbols (process-constructor form))
       
       (and (list? form) (= 'storage (first form)))
       (merge symbols (process-storage form))
       
       (and (list? form) (= 'functions (first form)))
       (merge symbols (process-functions form))
       
       :else
       symbols))
   {}
   (rest ast)))
