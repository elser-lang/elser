(ns evmlisp.compiler
  (:gen-class)
  (:require [clojure.string :as string]
            [evmlisp.errors :as errs]
            [evmlisp.core :as core]))

(def yul-builder
  {:contract-name nil
   :constructor-code []
   :runtime-code
   {:storage {:external [] :internal []}
    :functions {:external [] :internal []}}})

(defn defn-to-signature
  "Converts evmlisps's definitions to function signatures."
  [fn-name args]
  (format "%s(%s)" fn-name
          ;; Get all types of a function defintion.
          (string/join ","
                       (map name
                            (map (fn [v] (get (vec v) 1)) args)))))

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

(defn s-exp-to-yul
  "Produces Yul operation on given s-expressions."
  [body]
  body)

(defn inner-to-symbols
  "Convert language defintions to common symbol-table data."
  [definition external?]
  (if (nil? definition) (vec nil)
      (let [operation (first definition)
            def-name (first (rest definition))
            types (rest (rest definition))]
        
        (cond
          ;; Parse function definitions.
          (= operation 'defn)          
          (let [fn-body (nth types (- (count types) 1))
                args (first types)
                return (nth types 1)
                signature (defn-to-signature def-name args)]
            
            {
             :name def-name             
             :signature signature             
             :args (args-to-symbols args)
             :body (s-exp-to-yul fn-body)
             :returns (map (fn [v] {:type (name v)}) return)})

          ;; Storage variable definition
          (= operation 'def)
          {
           :name def-name
           :signature (if external? "todo:external-sig" nil)
           }
          ))))

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

;; TODO: do this thing better.
;;~~~~ MERGE THESE INTO 1 FUNCTION!
(defn storage-to-symbols [definitions]
  {:storage
   {
    :external (map (fn [d] (inner-to-symbols d true))
                   (:external definitions))
    
    :internal (map (fn [d] (inner-to-symbols d false))
                   (:internal definitions))}})

(defn fns-to-symbols [definitions]
  {:functions
   {
    :external (map (fn [d] (inner-to-symbols d true))
                   (:external definitions))
    
    :internal (map (fn [d] (inner-to-symbols d false))
                   (:internal definitions))}})


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
    (let [fns (process-ex-in functions)]
      (fns-to-symbols fns)))

(defn process-storage [storage]
  (let [sto (process-ex-in storage)
        slot 0x00] ; TODO; properly assign storage slots.
    (storage-to-symbols sto)))

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
