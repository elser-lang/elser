(ns evmlisp.compiler
  (:gen-class))

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

(defn process-ex-in [objects]
  (let [x (first (rest objects))
        external (:external x)
        internal (:internal x)]
    {:external external
     :internal internal}))

(defn process-functions [functions]
    {:functions (process-ex-in functions)})

(defn process-storage [storage]
    {:storage (process-ex-in storage)})

(defn collect-symbols [ast]
  (reduce (fn [symbols form]
            (cond
              ;; Collect namespace.
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

(defn generate-yul [ast]
  (let [symbols (collect-symbols ast)
        builder ""] ; TODO: use real builder!

    ;; Main header.
    (str builder "object \"" (:ns symbols) "\" {")
    (println builder)
    
    ;; Generate runtime code.
    (str builder "object \"runtime\" {")
    (str builder "code {")
    
    ;; Dispatch switch
    (str builder "switch selector()")
    (doseq [f (get-in symbols [:functions :external])]
      ;; TODO: use 4 bytes of hash instead of name.
      (str builder "case %s // %s" (:name f) (:name f)))
    
    ;; TODO: translate function bodies
    
    (str builder "}") ; Close runtime
    (str builder "}") ; Close object
    ))


;; TODO: translate Lisp to Yul language, checking for proper syntax.
(defn compile-to-yul
  [ast]
  (generate-yul ast))
