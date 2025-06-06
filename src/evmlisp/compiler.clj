(ns evmlisp.compiler
  (:gen-class)
  (:require [clojure.string :as string]))

(def yul-builder
  {:contract-name nil
   :constructor-code []
   :runtime-code
   {:storage {:external [] :internal []}
    :functions {:external [] :internal []}}})

(defn def-to-symbol
  "Convert language defintions to common symbol-table data."
  [definition]
  (if (nil? definition) (vec nil)
      (let [operation (first definition)
            fn-name (first (rest definition))
            types (rest (rest definition))]
        (println ":ARGUMENTI" types)
        (cond
          (let [body (nth (- (count types)1))]
          ;; Function definition
          (= operation 'defn)
          ;;(let [args (first types)]
          ;; ∀ f ∈ :functions (rest (rest f)) => ([(arg0 :type) ... (argn :type)] :type)
          {:name fn-name
           
           :signature (format "%s(%s)" fn-name
                              ;; Get all types of a function defintion.
                              (string/join ","
                                           (map name
                                                (map (fn [v] (get (vec v) 1)) (first types)))))
           
           :args (map (fn [v] {
                              :name (get (vec v) 0)
                              :type (name (get (vec v) 1))})
                      (first types))
           :returns (format "fixme:%s" (last types)) ; TODO: properly produce return types.
           }

          ;; Storage variable definition
          (= operation 'def)
          ;; ∀ s ∈ :storage (rest (rest s))   => (:uint256 => ... =>)
          {
           :name fn-name
           ;:signature (rest types)
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
    :external (map (fn [d] (def-to-symbol d))
                   (:external definitions))
    
    :internal (map (fn [d] (def-to-symbol d))
                   (:internal definitions))}})

(defn fns-to-symbols [definitions]
  {:functions
   {
    :external (map (fn [d] (def-to-symbol d))
                   (:external definitions))
    
    :internal (map (fn [d] (def-to-symbol d))
                   (:internal definitions))}})


(defn process-ex-in
  [objects]
  (let [x (first (rest objects))
        external (:external x)
        internal (:internal x)]
    {:external external
     :internal internal}))

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
;     :body '(+ x x)
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
