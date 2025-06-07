(ns evmlisp.compiler
  (:gen-class)
  (:require [clojure.string :as string]
            [evmlisp.errors :as errs]
            [evmlisp.core :as core]))

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

; Stages:
; 1. Create main contract Object (from namespace):
;    object "Name" {
;      1.1. create "code" (constructor) if needed (get it from :constructor)y
;                            
;      code {                     
;         1.1.1. Do constructor operations.
;      }
;                     
;      1.2. Create runtime object (what will be deployed on-chain)
;              
;      object "runtime" {
;         1.2.1. Create runtime-code.
;         code {       
;             1.2.1.1. Initialize dispatcher for functions (iterate over :external fns).
;             switch selector()
;             case (str "0x" keccak256(name)[:8]) {
;                 - function_call(decode_arg0(), ..., decode_argN())
;                 - returnTrue() 
;             }
;             default {
;                 revert(0,0) /* No matching selector */
;             }
;         1.2.2. Add default helper functions (calldata [en/de]coding, etc.)
;         1.2.3. Add events.
;         1.2.4. Add storage vars.
;         1.2.3. Add herlper utility functions (if needed [e.g.: lte, gte, etc.]).
;         }
;      }
;    }

;(defn compile-form
;  "Used to compile function's body."
;  [form ctx])

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
                        (let [[_ name types & opts] def-form
                              ;; Use custom slot if specified, otherwise allocate new.
                              custom-slot (when (map? (last opts)) (:slot (last opts)))
                              slot (or custom-slot (:slot-counter state))
                              ;; Increment counter if using auto-allocation.
                              new-counter (if custom-slot 
                                            (:slot-counter state)
                                            (inc slot))
                              var-def {:name name
                                       :type (str types)
                                       :slot slot
                                       :visibility visibility}]
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

;~ Symbol-table draft
;{
; :ns "math",  => Defines namespaces (aka contract-name)
;                    
;~ For storage it's needed to define its layout and access functions.
;~ Plus for :external values it's needed to generate getters for them,
;~ so they will be part of the ABI => add to function dispatcher.
;                        
; :storage
;   {:external [{:slot nil :name nil, :type nil :size 0}],
;    :internal [
;     {:slot [CUSTOM_SLOT || COUNTER] ;; for storage pos we need to have counter.
;      :name "start"
;      :signature "start()",
;      :args [{:name "to", :type :address}, 
;            {:name "amount", :type :uint256}],
;      :returns :bool,
;      :size 32
;     ]},       
;               
;~ ∀ e ∈ :external => e ∈ dispatcher(e.selector, e.signature) => e ∈ ABI
;~ ∀ i ∈ :internal => i ∉ dispatcher & i ∉ ABI
;~ Thus for external functions we need to know their selector and signature
;~ and for internal we only need to know their signature.
;~ For signatures, we need to maintain a vector with argument types and return types.
;
; TODO: add 'receive()/fallback()' as standalone function.
; :functions {
;   :external [
;    {:name "transfer",
;     :selector "0xa9059cbb",
;     :signature "transfer(address,uint256)",
;     :body '(+ x x) ; (s-expressions)
;     :args [{:name "to", :type :address}, 
;            {:name "amount", :type :uint256}],
;     :returns :bool,
;     :mutability :nonpayable}
;  ]
;   :internal [
;    {:name "",
;     :selector "",
;     :signature "",
;     :args [],
;     :returns nil,
;     :mutability :payable}
;   ] ;; No internal functions here.
;}      
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

;; State update helpers
(defn update-section [builder section f & args]
  (update-in builder [section] #(apply f % args)))

(defn add-runtime-code [builder code]
  (update-section builder :runtime-code conj code))

(defn add-function [builder visibility fn-def]
  (update-section builder [:functions visibility] conj fn-def))

(defn generate-yul [builder]
  (let [{:keys [contract-name runtime-code functions]} builder]
    ;; TODO: add constructor code (if there's one).
    (str "object \"" contract-name "\" {\n"
         "  code {\n"
         "    datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))\n"
         "    return(0, datasize(\"runtime\"))\n"
         "  }\n"
         "  object \"runtime\" {\n"
         "    code {\n"
         ;;~~~~~~~  Dispatcher
         "      let selector := shr(224, calldataload(0))\n"
         "      switch selector\n"
         ;; TODO: add (dispatch-cases (:external functions))
         "      default { revert(0,0) }\n\n"
         ;;~~~~~~~ Runtime code
         (string/join "\n" runtime-code)
         "\n    }\n"
         "  }\n"
         "}")))


;; TODO: translate Lisp to Yul language, checking for proper syntax.
(defn compile-to-yul [ast]
  (collect-symbols ast))
