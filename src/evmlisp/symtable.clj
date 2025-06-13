(ns evmlisp.symtable
  (:gen-class)
  (:import [org.web3j.crypto Hash]
           [org.web3j.utils Numeric])
  (:require [clojure.string :as string]
            [evmlisp.errors :as errs]
            [evmlisp.core :as core]))

(defn sig->fn-call
  [sig]
  (let [[_ name args]
        (re-matches #"([^()]+)\((.*)\)" sig)
        types  (if (string/blank? args) [] (string/split args #","))
        arity (count types)
        blanks (repeat arity "%s")
        fmt-sig (str name "(" (string/join "," blanks) ")")]
    (fn [& args]
      (if (not (= (count args) arity))
        (errs/err-arity-exception name (count args) arity)
        (apply format fmt-sig args)))))

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


(defn def-to-signature
  "Converts evmlisps's external storage definitions to function signatures."
  [def-name sto-types]
  (format "%s(%s)" def-name
          (string/join ","
                       (map
                        (fn [t]
                          (name (first t))) sto-types))))

(defn args-to-symbols
  "
  Produces {:name ... :type ...} map on
  a given [(arg_0 :type) ... (arg_n :type)]
  "
  [args]
  (map-indexed (fn [i v]
         (let [named? (= (mod (count v) 2) 0)
               arg-name (if named? (nth v 0) (str "arg_" i))
               arg-type (name (last v))]
           {:name arg-name
            :type arg-type}))
         args))

(defn sto-var-type
  [sto-types]
  (cond
    (some #{'=>} sto-types)
    {:map true}

    :else
    {:simple true}))

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

;; TODO: all these 'process-*' functions should be combined into 1.
(defn process-constructor [constructor]
  {:constructor {:body (second constructor)}})

(defn process-constants [constants]
  (let [definitions (process-ex-in constants)]
    (let [initial-state {:constants
                         {:external [] :internal []}}]
      (reduce (fn [state [visibility defs]]
                (reduce (fn [state def-form]
                          (let [[_ c-name c-type val] def-form
                                sig (format "%s()" c-name)
                                var-def {:name c-name
                                         :selector (obtain-selector sig)
                                         :signature sig
                                         :fn-call sig
                                         :body val
                                         :return (map name c-type)}]
                            ;; Validate that name is capped.
                            (if (not (= (str c-name) (string/upper-case c-name)))
                              (errs/err-non-upper-case-const c-name)
                              
                              (-> state
                                  (update-in
                                   [:constants visibility] conj var-def)))))
                          state
                          defs))
              initial-state
              [[:external (:external definitions)]
               [:internal (:internal definitions)]]))))

(defn process-events [events] {:events ""})

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
                                       :fn-call (sig->fn-call sig)
                                       :args (args-to-symbols args)
                                       :fn-type (function-type fn-type)
                                       :body body
                                       :return (args-to-symbols (second ret))}]
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
                              ret (map name (subvec sto-types
                                                       (- (count sto-types) 1)))
                              t (sto-var-type sto-types)
                              sto-types (if (:map t)
                                           (reduce
                                            (fn [v t] (conj v (list t)))
                                            []
                                            (drop-last
                                             (remove #{'=>} sto-types)))
                                           '[])
                              sig (def-to-signature def-name sto-types)
                              ;; Increment counter if using auto-allocation.
                              new-counter (if custom-slot
                                            (:slot-counter state)
                                            (inc slot))
                              var-def {:name def-name
                                       :selector (obtain-selector sig)
                                       :signature sig
                                       :args (args-to-symbols sto-types)
                                       :slot slot
                                       :return ret
                                       :var-type t}]
                          (println "sto-types" sto-types)
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

(def valid-nested-type?
  {:list (fn [c]
           (if (not (list? (first (rest c))))
             (errs/err-invalid-nested-type (first c) (rest c) '())))

   :map (fn [c]
          (if (not (map? (first (rest c))))
            (errs/err-invalid-nested-type (first c) (rest c) '{})))

   :string (fn [c]
          (if (not (string? (first (rest c))))
            (errs/err-invalid-nested-type (first c) (rest c) 'string)))

   :symbol (fn [c]
             (if (not (symbol? (first (rest c))))
               (errs/err-invalid-nested-type (first c) (rest c) 'symbol)))
   })

(defn collect-symbols
  "Produces a symbol table on a given AST."
  [ast]
  (reduce
   (fn [symbols form]
     (cond
       (not (list? form))
       (errs/err-invalid-top-level-form form)
       
       ;; Namespace defintion.
       (= 'ns (first form))
       (do ((:symbol valid-nested-type?) form)
           (assoc symbols :ns (second form)))

       (= 'constructor (first form))
       (do ((:list valid-nested-type?) form)
           (merge symbols (process-constructor form)))

       (= 'events (first form))
       (do ((:list valid-nested-type?) form)
           (merge symbols (process-events form)))

       (= 'constants (first form))
       (do ((:map valid-nested-type?) form)
           (merge symbols (process-constants form)))
       
       (= 'storage (first form))
       (do ((:map valid-nested-type?) form)
           (merge symbols (process-storage form)))
       
       (= 'functions (first form))
       (do ((:map valid-nested-type?) form)       
           (merge symbols (process-functions form)))
       
       :else
       symbols))
   {}
   ast))
