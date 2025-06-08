(ns evmlisp.compiler
  (:gen-class)
  (:require [evmlisp.env :as env]
            [clojure.string :as string]))

(declare compile)

(defn compile-symbols
  [symbols yulenv]
  (cond
    (symbol? symbols) (env/eget yulenv symbols)
    
    (map? symbols) (let [k (keys symbols)
                     v (doall (map (fn [x] (compile x yulenv)) (vals symbols)))]
                 (zipmap k v))
    
    (seq? symbols) (doall (map (fn [x] (compile x yulenv)) symbols))
    
    (vector? symbols) (vec (doall
                        (map (fn [x] (compile x yulenv)) symbols)))
    
    :else symbols))

(defn compile
  [symbols yulenv]
  (cond
    (nil? symbols) symbols
    
    (not (seq? symbols)) (compile-symbols symbols yulenv)
    
    (empty? symbols) '()
    
    (seq? symbols) (let [f (first symbols)]
                 (cond

                   ;; 'def' - call the set method of the current environment
                   ;; (second parameter of compile called env) using the
                   ;; unevaluated first parameter (second list element)
                   ;; as the symbol key and the evaluated
                   ;; second parameter as the value.
                   (= f 'def!)
                   (let [k (second symbols)
                         v (compile (nth symbols 2) yulenv)]
                     (env/eset yulenv k v))                   

                   ;; 'let' - create a new environment using the
                   ;; current environment as the outer value and
                   ;; then use the first parameter as a list of
                   ;; new bindings in the "let*" environment.
                   (= f 'let*)
                   (let [let-env (env/env yulenv)]
                     (doseq [[b e] (partition 2 (first (rest symbols)))]
                       (env/eset let-env b (compile e let-env)))
                     (compile (nth symbols 2) let-env))

                   ;; 'do' - evaluate all the elements of the list
                   ;; and return the final evaluated element.
                   (= f 'do)
                   (symbols (compile-symbols (rest symbols) yulenv))

                   ;; 'if' - good old if statement.
                   (= f 'if)
                   (let [exprs (rest (rest symbols))]
                     (if (compile (second symbols) yulenv)
                       (compile (first exprs) yulenv)

                       (if (= (count exprs) 2)
                         (compile (second exprs) yulenv)
                         nil)))

                   ;; 'fn' - return a new function closure.
                   (= f 'fn*)
                   (let [p (second symbols)
                         e (nth symbols 2)]
                     (fn [& args]
                       (compile e (env/env yulenv p (or args '() )))))
                   
                   :else
                   (let [l' (compile-symbols symbols yulenv)
                         f (first l')
                         args (rest l')]
                     (apply f args))))))

(defn get-dispatcher [definitions]
  (reduce (fn [dispatcher definition]
            (str dispatcher
                 (str
                  "      case " (:selector definition) " { /*" (:signature definition) "*/\n"
                  "            return(0,0) // TODO: implement body\n"
                  "      }\n")))
          ""
          definitions))


(defn generate-yul [symbols yulenv]
  (let [contract-name (:ns symbols)
        constructor (:constructor symbols)
        storage (:storage symbols)
        functions (:functions symbols)]
    (println "name" contract-name)
    (println "storage" storage)
    ;; TODO: add constructor code (if there's one).
    (str "object \"" contract-name "\" {\n"
         "  code {\n"
         "    datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))\n"
         "    return(0, datasize(\"runtime\"))\n"
         "  }\n"
         "  object \"runtime\" {\n"
         "    code {\n"
         ;;~~~~~~~  Dispatcher
         "      // Dispatcher\n"
         "      switch shr(224, calldataload(0))\n"
         (get-dispatcher (:external functions))         
         (get-dispatcher (:external storage))
         "      default { revert(0,0) }\n\n"
         ;;~~~~~~~ Runtime code
         "" ;; TODO: add runtime code generation.
         "\n    }\n"
         "  }\n"
         "}")))


(defn symtable-to-yul
  [symbols yulenv]
  (generate-yul symbols yulenv))

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
