(ns elser.symtable
  (:gen-class)
  (:import [org.web3j.crypto Hash]
           [org.web3j.utils Numeric])
  (:require [clojure.string :as string]
            [elser.errors :as errs]
            [elser.types :as els-types]
            [elser.destruct :as destruct]
            [elser.core :as core]))

(def STO_ACCESS_LOWER_BOUND 0)
(def STO_ACCESS_UPPER_BOUND 3)

(defn check-access-bounds [o]
  (<= STO_ACCESS_LOWER_BOUND o STO_ACCESS_UPPER_BOUND))

(defn validate-permissions [write read]
  (if (not (and (int? write) (int? read)))
    (errs/err-sto-access-non-int [write read]))
  
  (if (not (and (check-access-bounds write)
               (check-access-bounds read)))
    (errs/err-invalid-permission-value
     [write read]
     [STO_ACCESS_LOWER_BOUND
      STO_ACCESS_UPPER_BOUND])))

(def valid-form?
  {:list (fn [c]
           (if (not (list? (first (rest c))))
             (errs/err-invalid-nested-type (first c) (rest c) '())))

   :vec (fn [c]
           (if (not (vector? (first (rest c))))
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

(defn create-fn-call
  "
  Return lambda function that will construct Yul call to
  a function given its name and args.
  "
  [name args]
  (let [arity (count args)
        blanks (repeat arity "%s")
        fmt-sig (str name "(" (string/join "," blanks) ")")]
    
    (fn [& args]
        (if (not (= (count args) arity))
          (errs/err-arity-exception name (count args) arity)
          (apply format fmt-sig args))))
  )

(defn obtain-hash [val stringify?]
  (let [hash-bytes (Hash/sha3 (.getBytes val))]
    (if stringify?
      (Numeric/toHexString hash-bytes)
      hash-bytes)))

(defn obtain-selector
  "Converts a Solidity function signature to a 4-byte selector"
  [signature]
  (let [hash-bytes (obtain-hash signature false)
        selector-bytes (byte-array 4)]
    (System/arraycopy hash-bytes 0 selector-bytes 0 4)
    (Numeric/toHexString selector-bytes)))

(defn defn-to-signature
  "Converts elsers's definitions to function signatures."
  [fn-name args]
  (format "%s(%s)" fn-name
          ;; Get all types of a function defintion.
          (string/join ","
                       (map (fn [v]
                              ((get (vec v) 1) ; TODO: use record
                               els-types/to-sol-types
                               )) args))))

(defn def-to-signature
  "Converts elsers's external storage definitions to function signatures."
  [def-name args]    
  (format "%s(%s)" def-name
          (string/join ","
                       (map
                        (fn [a]
                          ((:type a) els-types/to-sol-types)) args))))

(defn args-to-symbols
  "
  Produces {:name ... :type ...} map on
  a given ((arg_0 [mut] :type) ... (arg_n [mut] :type))
  "
  [args]
  (map-indexed (fn [i v]
         (let [mutable? (some #{'mut} v)
               arg-name (nth v 0)
               arg-type (last v)]
           {:name arg-name
            :type arg-type
            :mutable? mutable?}))
         args))

(defn extract-external-internal
  "Extract :external & :internal definitions from top-level object"
  [object]
  
  (:list valid-form? (first (rest object))) ; Verify that it's a list.
  
  (let [x (apply hash-map (first (rest object)))
        ex (:external x)
        in (:internal x)] ; Convert list to a map.
    x))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ PROCESSING FUNCTIONS

(defn process-constructor [constructor]
  (let [body (rest constructor)]
    (if (> (count body) 1)
      (errs/err-invalid-nested-constr-form body '()))
    {:constructor {:body (second constructor)}}))

(defn process-constants [constants]
  (let [definitions (extract-external-internal constants)]
    (let [initial-state {:constants
                         {:external [] :internal []}}]
      (reduce (fn [state [visibility defs]]
                (reduce (fn [state def-form]
                          
                          (let [destructured (destruct/destruct-def def-form)
                                const-name (:name destructured)
                                sig (format "%s()" const-name)
                                var-def {:name const-name
                                         :selector (obtain-selector sig)
                                         :signature sig
                                         :fn-call sig
                                         :body (:opts destructured)
                                         :type (:type destructured)
                                         :return (:ret destructured)}]
                            ;; Validate that name is capped.
                            (if (not (= (str const-name) (string/upper-case const-name)))
                              (errs/err-non-upper-case-const const-name)
                              
                              (-> state
                                  (update-in
                                   [:constants visibility] conj var-def)))))
                          state
                          defs))
              initial-state
              [[:external (:external definitions)]
               [:internal (:internal definitions)]]))))

(defn process-events [events]
  (let [initial-state {:events []}
        definitions (last events)]
    (reduce (fn [state def-form]
              (let [[_ event-name args] def-form
                    sig (defn-to-signature event-name args)
                    arguments (args-to-symbols args)
                    var-def {:name event-name
                             :sig-hash (obtain-hash sig true)
                             :signature sig
                             :fn-call (create-fn-call event-name arguments)
                             :args arguments}]
                (-> state
                    (update-in [:events] conj var-def))))
            initial-state
            definitions)))

(defn process-functions [functions]
  (let [definitions (extract-external-internal functions)]
    (let [initial-state {:functions
                         {:external [] :internal []}}]

      (reduce (fn [state [visibility defs]]
                (reduce (fn [state def-form]
                          (let [[fn-type fn-name args access ret body] def-form
                                access (apply hash-map (rest access))
                                write (:w access)
                                read (:r access)
                                sig (defn-to-signature fn-name args)
                                var-def {:name fn-name
                                         :selector (obtain-selector sig)
                                         :signature sig
                                         :permissions access
                                         :fn-call (create-fn-call fn-name (args-to-symbols args))
                                         :args (args-to-symbols args)
                                         :body body
                                         :return (args-to-symbols (second ret))}]
                            (validate-permissions write read)
                            (create-fn-call fn-name (args-to-symbols args))
                            (-> state
                                (update-in 
                                 [:functions visibility] 
                                 conj var-def))))
                        state
                        defs))
              initial-state
              [[:external (:external definitions)]
               [:internal (:internal definitions)]]))))

(defn process-storage [storage]
  (let [definitions (extract-external-internal storage)
        initial-state {:slot-counter 0x00
                       :storage {:external [] :internal []}
                       :occupied-slots []}]
    
    (reduce (fn [state [visibility defs]]
              
              (reduce (fn [state def-form]
                        
                        (let [destructured (destruct/destruct-def def-form)

                              ;; Use custom slot if specified, otherwise allocate new.
                              custom-slot (:opts destructured)
                              slot (or (:slot custom-slot) (:slot-counter state))
                              sig (def-to-signature
                                    (:name destructured)
                                    (:args destructured))

                              ;; Increment counter if using auto-allocation.
                              new-counter (if custom-slot
                                            (:slot-counter state)
                                            (inc slot))
                              var-def {:name (:name destructured)
                                       :selector (obtain-selector sig)
                                       :signature sig
                                       :args (:args destructured)
                                       :slot slot
                                       :fn-call (create-fn-call 
                                                 (:name destructured)
                                                 (:args destructured))
                                       :type (:type destructured)
                                       :return (:ret destructured)}]

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

(defn process-transient [trn]
  {:transient (process-storage `(storage (:internal ~(last trn))))})

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
       (do ((:symbol valid-form?) form)
           (assoc (assoc symbols :pragma (last (last form)))
                  :ns (second form)))

       (= 'constructor (first form))
       (do ((:list valid-form?) form)
           (merge symbols (process-constructor form)))

       (= 'events (first form))
       (do ((:list valid-form?) form)
           (merge symbols (process-events form)))

       (= 'constants (first form))
       (do ((:list valid-form?) form)
           (merge symbols (process-constants form)))

       (= 'transient (first form))
       (do ((:list valid-form?) form)
           (merge symbols (process-transient form)))
       
       (= 'storage (first form))
       (do ((:list valid-form?) form)
           (merge symbols (process-storage form)))
       
       (= 'functions (first form))
       (do ((:list valid-form?) form)       
           (merge symbols (process-functions form)))
       
       :else
       symbols))
   {}
   ast))
