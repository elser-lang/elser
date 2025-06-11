(ns evmlisp.compiler
  (:gen-class)
  (:require [evmlisp.env :as env]
            [evmlisp.symtable :as symtable]
            [evmlisp.errors :as errs]
            [evmlisp.env :as env]            
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(defn init-memory-env
  [yul-env]
  ;; Init memory env, using yul-env as outer env.
  ;; Initially memory env is empty, and will
  ;; be filled with local variables (if any).
  (env/env yul-env))

;; fix: this function creates calldata env even for internal function calls
;;      whose arguments aren't stored in calldata.
(defn init-calldata-env
  [yul-env definition]
  ;; Calldata env is on the bottom of env hierarchy.
  (let [calldata-env (env/env yul-env)]
    (doseq [[k v]
            (reduce (fn [full x] (conj
                                 full
                                 [(:name x) (:name x)]
                                 ))
                    [] (:args definition))
            ] (env/eset calldata-env k v))
    calldata-env))

(defn init-local-env
  [yul-env definition]
  (init-calldata-env
   (init-memory-env yul-env)
   definition))

(defn init-storage-env
  "
  Storage env is separate, because iteractions with it
  are made via (sto/... ) commands.
  "
  [symbols sto-ns]
  (let [sto-env (env/env)
        sto (into (:external (:storage symbols))
                  (:internal (:storage symbols)))
        sto-defs (reduce (fn [full x]
                           (conj full
                                 [(:name x) {:args (:args x) :slot (:slot x)}])) [] sto)
        ]
    (do
      ;; Set core storage operations.
      (doseq [[k v] sto-defs] (env/eset sto-env k v))
      (doseq [[k v] sto-ns] (env/eset sto-env k v)))
    sto-env))

(declare compile)

(defn compile-symbols
  [symbols yul-env sto-env]
  (println "symbols" symbols)
  (cond
    (symbol? symbols) (env/eget yul-env symbols)
    
    (map? symbols) (let [k (keys symbols)
                         v (doall (map (fn [x] (compile x yul-env sto-env))
                                       (vals symbols)))]
                     (zipmap k v))
    
    (seq? symbols) (doall (map (fn [x] (compile x yul-env sto-env))
                               symbols))
    
    (vector? symbols) (vec (doall
                            (map (fn [x] (compile x yul-env sto-env))
                                 symbols)))
    
    :else symbols))

(defn compile
  [symbols yul-env sto-env]
  (println "compile(symbols)" symbols)
  (cond
    ;; TODO: what to do wtih this case?
    (nil? symbols) symbols
    
    (not (seq? symbols)) (compile-symbols symbols yul-env sto-env)
    
    (empty? symbols) '()
    
    (seq? symbols) (let [f (first symbols)]
                     (cond

                       ;; 'let' - create a new environment using the
                       ;; current environment as the outer value and
                       ;; then use the first parameter as a list of
                       ;; new bindings in the "let*" environment.
                       (= f 'let)
                       (let [let-env (env/env yul-env)]
                         (doseq [[b e] (partition 2 (first (rest symbols)))]
                           (env/eset let-env b (compile e let-env)))
                         (compile (nth symbols 2) let-env))

                       ;; 'do' - evaluate all the elements of the list
                       ;; and return the final evaluated element.
                       (= f 'do)
                       (string/join "\n"
                                    (mapv (fn [sym] (compile sym yul-env sto-env))
                                          (rest symbols)))
                       
                       ;; 'if' - good old if statement.
                       (= f 'if)
                       (let [exprs (rest (rest symbols))]
                         (if (compile (second symbols) yul-env)
                           (compile (first exprs) yul-env)

                           (if (= (count exprs) 2)
                             (compile (second exprs) yul-env)
                             nil)))

                       ;; 'fn' - return a new function closure.
                       (= f 'fn)
                       (let [p (second symbols)
                             e (nth symbols 2)]
                         (fn [& args]
                           (compile e (env/env yul-env p (or args '() )))))

                       ;; 'sto' - a storage-access function.
                       ;; TODO: sstore(...) should use slot
                       ;; not variable name!
                       (= f 'sto)
                       (let [op (second symbols)
                             sto-var (nth symbols 2)
                             var-slot (:slot (env/eget sto-env sto-var))]
                         (if (= op 'write!)
                           (apply (env/eget sto-env op)
                                  [(:slot (env/eget sto-env sto-var))
                                   (compile-symbols
                                    (last symbols)
                                    yul-env sto-env)])

                           (apply (env/eget sto-env op) [sto-var])))
                         
                       :else
                       (do (println "else case")
                           (let [l' (compile-symbols symbols yul-env sto-env)
                                 f (first l')
                                 args (rest l')]
                             (apply f args)))))))

;; 0x54f363a300000000000000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000000005
(declare evmlisp-args->yul-args)

(defn compile-calldataload-for-args [definition tabs]
   (if (empty? (:return definition))

     ; Constructs 'fn_name(...)'
     (str (string/join "" (repeat (* 4 tabs) " "))
          (:name definition) "("
          (loop [args-load []
                 start-offset 4
                 args-count (count (:args definition))]
            (cond
              (= args-count 0) (string/join ", " args-load)
              :else (recur
                     (conj args-load
                           (format "calldataload(%s)" start-offset))
                     (+ start-offset 32)  ; todo: need to use real size!
                     (- args-count 1)))) ")")

     ;; Recursively produce calldata arguments if it's needed
     ;; to store return value in memory.
     ;; Constructs 'mstore(0, fn_name(...))'
     (str "        mstore(0, " (compile-calldataload-for-args
                                (dissoc definition :return) 0) ")")))


(defn compile-top-level-return [definition]
  (if (empty? (:return definition))
    ""
    "        return(0,32)\n")) ;; TODO: implement

;; TODO: handle dynamics data types.
(defn compile-storage-var-body [definition yul-env]
    (format "          ret_val := sload(%s)\n" (:slot definition)))

(defn compile-function-body
  [definition yul-env sto-env]
  (compile (:body definition)
           yul-env
           sto-env))

(defn evmlisp-args->yul-args
  [args]
  (string/join
   ","  
   (reduce
    (fn [whole a]
      (conj whole
            (str (:name a)))) [] args)))

(defn evmlisp-func-body->yul-func-body
  [definition yul-env sto-env]
  (let [storage? (contains? definition :slot)]
    (if storage?
      ;; Storage getters will just use `sload()`
      (compile-storage-var-body definition yul-env)
      (compile-function-body definition yul-env sto-env))))

(defn evmlisp-ret->yul-ret [ret]
  (let [return-count (count ret)]
    (cond      
      (> return-count 1)
      errs/err-gt1-return

      (= return-count 1)
      " -> ret_val "

      :else
      " ")))

(defn evmlisp-func->yul-func
  "Translates individual evmlisp function into a Yul function."
  [definition yul-env sto-env]
  (str
   "      function " (:name definition) "("
   ;; 1) Compile arguments + returns.
   (evmlisp-args->yul-args (:args definition)) ")" (evmlisp-ret->yul-ret
                                                    (:return definition)) "{\n"
   ;; 2) Compile function body.
   (evmlisp-func-body->yul-func-body
    definition
    (init-local-env yul-env definition)
    sto-env)
   "      }\n"))

(defn constructor-code [constructor yul-env]
  "    // Constructor is unsupported rn\n")

(defn dispatcher-code [definitions yul-env]
  (reduce (fn [dispatcher definition]
            (str dispatcher
                 (str
                  "      case " (:selector definition) " /* " (:signature definition) "*/ {\n"
                  (compile-calldataload-for-args definition 2) "\n"
                  (compile-top-level-return definition)
                  "      }\n")))
          ""
          definitions))

;; TODO: utilzie other function-creation methods!
(defn storage-getters-code
  "Generates storage access functions."
  [ex-sto yul-env]
  (reduce (fn [functions definition]
            (str functions
                 (evmlisp-func->yul-func
                  definition yul-env '{}))) "" ex-sto))

(defn functions-code
  [definitions yul-env sto-env]
  (reduce (fn [functions definition]
            (str functions
                 (evmlisp-func->yul-func definition yul-env sto-env)))
          ""
          definitions))

(defn compile-to-yul
  "This function is doing a template-based Yul code generation."
  [symbols yul-env sto-env]  
  (let [contract-name (:ns symbols)
        constructor (:constructor symbols)
        storage (:storage symbols)
        functions (:functions symbols)]
    ;; TODO: add constructor code (if there's one).
    (str "object \"" contract-name "\" {\n"
         "  code {\n"
         (constructor-code constructor yul-env)
         "    datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))\n"
         "    return(0, datasize(\"runtime\"))\n"
         "  }\n"
         "  object \"runtime\" {\n"
         "    code {\n"         
         ;;~~~~~~~  Dispatcher
         "      // Dispatcher\n"
         "      switch shr(224, calldataload(0))\n"
         (dispatcher-code (:external functions) yul-env)
         "      // Storage-access dispatcher\n"         
         (dispatcher-code (:external storage) yul-env)
         "      default { revert(0,0) }\n\n"
         "\n"
         (functions-code (:external functions)
                         yul-env sto-env)
         (functions-code (:internal functions)
                         yul-env sto-env)
         "\n"
         "      /* -------- external storage access ---------- */\n"
         (storage-getters-code (:external storage) yul-env)
         "      /* -------- internal storage access ---------- */\n"
         (storage-getters-code (:internal storage) yul-env)         
         "\n    }\n"
         "  }\n"
         "}")))

;; TODO: MUST extend `yul-env` with :outer envs that will contains:
;; - storage binds
;; - memory binds
;; - calldata binds
(defn symtable-to-yul
  [symbols yul-env sto-ns]
  (let [sto-env (init-storage-env symbols sto-ns)]
    (compile-to-yul symbols yul-env sto-env)))
