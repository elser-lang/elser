(ns evmlisp.symtable
  (:gen-class)
  (:require [clojure.string :as string]
            [evmlisp.errors :as errs]
            [evmlisp.core :as core]))

;; TODO: implement.
(defn fn-selector [sig]
  "0x00000000")

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
  [def-name types]
  (format "%s(%s)" def-name
          (string/join ","
                       (map name
                            (map (fn [v] (get (vec v) 1)) types)))))

(defn args-to-symbols
  "
  Produces {:name ... :type ...} map on
  a given [(arg_0 :type) ... (arg_n :type)]
  "
  [args]
  (map (fn [v]
         {:name (get (vec v) 0)
          :type (name (get (vec v) 1))})
       args))

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
                        (let [[_ name args ret body] def-form
                              sig (defn-to-signature name args)
                              var-def {:name name
                                       :selector (fn-selector sig)
                                       :signature sig
                                       :args (args-to-symbols args)
                                       :body body
                                       :return ret}]
                          (-> state
                              (update-in [:functions visibility] conj var-def))))
                      state
                      defs))
            initial-state
            [[:external (:external definitions)]
             [:internal (:internal definitions)]]))))

;; FIX: this function:
;; - doesn't detect mappings.
;; - can't increment storage counter for data that occupies more than 32 bytes.
;; - looks ugly...
(defn process-storage [storage]
  (let [definitions (process-ex-in storage)
        initial-state {:slot-counter 0x00
                       :storage {:external [] :internal []}
                       :occupied-slots []}]
    (reduce (fn [state [visibility defs]]
                                        ; Iterate over all :external storage vars.
                                        ; And then over all :internal storage vars.
              (reduce (fn [state def-form]
                        (let [[_ def-name sto-types & opts] def-form
                              ;; Use custom slot if specified, otherwise allocate new.
                              custom-slot (when (map? (last opts)) (:slot (last opts)))
                              slot (or custom-slot (:slot-counter state))
                              ;; Increment counter if using auto-allocation.
                              new-counter (if custom-slot 
                                            (:slot-counter state)
                                            (inc slot))
                              var-def {:name def-name
                                       :type (map name sto-types)
                                       :slot slot}]
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
