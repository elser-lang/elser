(ns elser.compiler
  (:gen-class)
  (:require [elser.env :as env]
            [elser.symtable :as symtable]
            [elser.errors :as errs]
            [elser.env :as env]
            [elser.destruct :as destruct]
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(defn add-funcs-to-env
  "
  Add function name => function call to the main env.
  "
  [symbols yul-env]
  (let [env-w-fns (env/env yul-env)
        ;; fix: ugly code.
        funcs (into (:events symbols)

                    (into (into (:external (:constants symbols))
                                (:internal (:constants symbols)))
                          
                          (into (:external (:functions symbols))
                                (:internal (:functions symbols)))))
        func-defs (reduce (fn [full x]
                            (conj full
                                  [(:name x) (:fn-call x)])) [] funcs)
        ]
    (doseq [[k v] func-defs] (env/eset env-w-fns k v))
    env-w-fns))

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
                    [] (into (:return definition) (:args definition)))
            ] (env/eset calldata-env k v))
    calldata-env))

(defn init-local-env [yul-env definition]
  (init-calldata-env yul-env definition))

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
                                 [(:name x) {:args (:args x)
                                             :slot (:slot x)
                                             :type (:type x)
                                             :fn-call (:fn-call x)
                                             :name (:name x)
                                             :return (:return x)}])) [] sto)
        ]
    (do
      ;; Set core storage operations.
      (doseq [[k v] sto-defs] (env/eset sto-env k v))
      (doseq [[k v] sto-ns] (env/eset sto-env k v)))
    sto-env))

(declare compile)

(defn compile-symbols
  [symbols yul-env sto-env]
  (cond
    (symbol? symbols) (env/eget yul-env symbols)
    
    (map? symbols) (let [k (keys symbols)
                         v (doall (map (fn [x] (compile x yul-env sto-env))
                                       (vals symbols)))]
                     (zipmap k v))
    
    (seq? symbols) (mapv (fn [x] (compile x yul-env sto-env))
                         symbols)
    
    (vector? symbols) (mapv (fn [x] (compile x yul-env sto-env))
                            symbols)
    
    :else symbols))

(defn compile
  [symbols yul-env sto-env]
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
                       (let [let-env (env/env yul-env)
                             local-defs (partition 2 (first (rest symbols)))
                             yul-lets (string/join
                                       "\n" (reduce
                                             (fn [full l]
                                               (conj full
                                                     (format
                                                      "let %s := %s"
                                                      (first l)
                                                      (compile
                                                       (nth l 1)
                                                       let-env
                                                       sto-env
                                                       ))))
                                             [] local-defs))]
                         ;; Bind local variables.
                         (doseq [[b _] local-defs]
                           (env/eset let-env b b))
                         ;; Execute 'let' body.
                         (str yul-lets "\n"
                              (string/join "\n"
                                           (mapv (fn [i] (compile (nth symbols i)
                                                                 let-env sto-env))
                                                 (range 2 (count symbols))))))

                       ;; 'do' - evaluate all the elements of the list
                       ;; (it doesn't return anything).
                       (= f 'do)
                       (let [exprs (mapv
                                    (fn [sym] (compile
                                              sym
                                              yul-env
                                              sto-env
                                              ))
                                    (rest symbols))
                             l (- (count exprs) 1)
                             last-expr (get exprs l)]
                         (string/join "\n" exprs))

                       ;; 'invoke!' - function invocation.
                       (= f 'invoke!)
                       (compile (rest symbols) yul-env sto-env)

                       ;; 'loop' in Yul (loop [binds] (cond) (post-iter) (body))
                       ;; in elser (loop [binds] (cond) (body) (post-iter)).
                       (= f 'loop)
                       (let [loop-env (env/env yul-env)
                             ;; Compile bindings like 'let'.
                             yul-lets (compile
                                       (list 'let (first (rest symbols)) nil)
                                       loop-env sto-env)
                             [_ _ cnd body post-iter] symbols
                             slt (assoc (vec symbols) 0 'let)]
                         ;; Validate that all parts of the 'loop' are present.
                         (if (some #{true} (map nil? [cnd body post-iter]))
                           (errs/err-incorrect-loop-def))

                         (if (not (= (first cnd) 'while))
                           (errs/err-incorrect-loop-part (first cnd) 'while))

                         (if (not (= (first post-iter) 'step))
                           (errs/err-incorrect-loop-part (first post-iter) 'step))
                         
                         (doseq [[b _] (partition 2 (first (rest symbols)))]
                           (env/eset loop-env b b))

                         (format "for { %s } %s { %s }\n { %s }\n"
                                 yul-lets
                                 (compile (second cnd) loop-env sto-env)
                                 ;; Execute 'step' block as 'do'.
                                 (compile (cons 'do (rest post-iter))
                                          loop-env sto-env)
                                 (compile body loop-env sto-env)))

                       (= f 'transfer*)
                       (let [to (compile (second symbols) yul-env sto-env)
                             value (compile (last symbols) yul-env sto-env)]
                         (format "pop(call(gas(),%s,%s,0,0,0,0))" to value))
                       

                       ;; 'sto' - a storage-access keyword.
                       (= f 'sto)
                       (let [op (second symbols)
                             sto-var (nth symbols 2)
                             var-slot (:slot (env/eget sto-env sto-var))]
                         (cond
                           (or (= op 'write!) (= op 'read!))
                           (apply (env/eget sto-env op)
                                  (list
                                   (env/eget sto-env sto-var)
                                   
                                   (compile-symbols (nthrest symbols 3)
                                            yul-env sto-env)))

                           (= op 'read!)
                           (apply (env/eget sto-env op)
                                  (list (env/eget sto-env sto-var) ))

                           :else
                           (errs/err-bind-not-found op)))
                       
                       :else
                       (let [l' (compile-symbols symbols yul-env sto-env)
                             f (first l')
                             args (rest l')]
                         (apply f args))))))

(declare elser-args->yul-args)

(defn compile-calldataload-for-args [definition tabs]
  (if (empty? (:return definition))
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
    "        return(0,32)\n"))


;; TODO: pad the value to 32 bytes.
(defn compile-map-offset-body
  "Return calculation of a storage offset for a map as `keccak256(h(k) . p)`"
  [definition]
  (loop [code-lines []
         slot (:slot definition)
         mem-counter 0
         args (:args definition)]
    (if (= (count args) 0)

      ;; Return offset value.
      (str (string/join "\n" code-lines) "\n"
           (format "          %s := %s" 
                   (:name (first (:return definition)))
                   slot) "\n")
      
      (recur
       (conj code-lines
             ;; HASH of concatenated KEY and SLOT
             (format "
          mstore(%s, %s) mstore(%s, %s)
          let %s := keccak256(%s, %s)
"
                     ;; mstore(offset, key)
                     mem-counter (:name (first args))
                     ;; mstore(offset+32, slot)
                     (+ mem-counter 32) slot
                     ;; let offset_x := keccak256(offset, size)
                     (str "offset_" mem-counter) mem-counter 64
                     ))
       (str "offset_" mem-counter); Updated slot equals to the value of OFFSET
       (+ mem-counter 64)
       (rest args)
       ))))

;; TODO: handle lists.
(defn compile-storage-var-body [definition yul-env]
  (let [t (:type definition)
        return (first (:return definition))]
    (cond
      ;; (:u256 :i256 :b32 :addr) stored in storage.
      (= t destruct/base-type)
      (format "          %s := sload(%s)\n"
              (:name
               (first
                (:return definition))) (:slot definition))

      (= t destruct/map-type)
      (format "          %s := sload(%s)\n"
              (:name
               (first
                (:return definition)))
              ;; sload(offset_func(args...))
              (apply
               (:fn-call definition)
               (map (fn [a] (:name a)) (:args definition))
               ))
      )))

(defn compile-function-body
  "Compile list of s-expressions of Elser function to Yul operations."
  [definition yul-env sto-env]
  (let [defn-body (:body definition)
        body (compile defn-body yul-env sto-env)
        lines (string/split body #"\n")]
    body))

(defn elser-args->yul-args
  [args]
  (string/join
   ","  
   (reduce
    (fn [whole a]
      (conj whole
            (str (:name a)))) [] args)))

(defn compile-event-body
  "
  Return event as a Yul function to invoke that executes
  LOGx opcode with variable number of arguments read from memory.
  "
  [definition]
  (loop [code-lines []
         mem-counter 0
         args (:args definition)]
    (if (= (count args) 0)
      
      (str (string/join "\n" code-lines) "\n"
           (format "          log0(0,%s)" mem-counter) "\n")
      
      (recur
       (conj code-lines
             (format "          mstore(%s, %s)"
                     mem-counter (:name (first args))))
       (+ mem-counter 32)
       (rest args)
       ))))

(defn elser-func-body->yul-func-body
  "
  Produce 3 possible types of compiled function body based on def-type:
  1. defn body of extneral or internal function.
  2. def body of an external storage vairable.
  3. def body of an external constant.
  "
  [definition yul-env sto-env def-type]
  (cond
    (contains? def-type :functions)
    (compile-function-body definition yul-env sto-env)
    
    (contains? def-type :storage) ; Storage getters will just use `sload()`
    (compile-storage-var-body definition yul-env)

    (contains? def-type :constants)
    (format "          %s := %s\n" (:name
                                    (first
                                     (:return definition))) (:body definition))

    (contains? def-type :events)
    (compile-event-body definition)))

(defn elser-ret->yul-ret [ret]
  (if (empty? ret)
    ""
    (format " -> %s"
            (string/join
             ", "
             (mapv (fn [r] (if (map? r)
                            (:name r)
                            "ret_val")) ret)))))

(defn elser-func->yul-func
  "Compile Elser's s-expressions to Yul operations, based on def-type."
  [definition yul-env sto-env def-type]
  
  ;; TODO: separete this special case better.
  (str
   
   (if (= (:type definition) destruct/map-type)
    ;; Create special offset calculating function.
    (str
     "      function " (:name definition) "_sto_offset("
     ;; Compile arguments + returns.
     (elser-args->yul-args (:args definition)) ")" (elser-ret->yul-ret
                                                    (:return definition)) "{\n"
     ;; Compile offset calculation.
     (compile-map-offset-body definition)
     "      }\n")
    )
  
  (str
   "      function " (:name definition) "("
   ;; Compile arguments + returns.
   (elser-args->yul-args (:args definition)) ")" (elser-ret->yul-ret
                                                  (:return definition)) "{\n"
   ;; Compile function body.
   (elser-func-body->yul-func-body definition
                                   (init-local-env yul-env definition)
                                   sto-env def-type)
   "      }\n")))

(defn constructor-code [constructor yul-env sto-env]
  (if (nil? constructor)
    ""
    (compile-function-body constructor yul-env sto-env)))

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

(defn functions-code
  "Return Yul function for each definition inside definitions vector."
  [definitions yul-env sto-env def-type]
  (reduce (fn [full definition]
            (str
             full (elser-func->yul-func
                   definition yul-env sto-env def-type))) "" definitions))

(defn compile-to-yul
  "Template-based Yul code generation."
  [symbols yul-env sto-env]  
  (let [contract-name (:ns symbols)
        constructor (:constructor symbols)
        events (:events symbols)
        constants (:constants symbols)
        storage (:storage symbols)
        functions (:functions symbols)]
    (str
     ;; Constructor code.
     "object \"" contract-name "\" {\n"
     "  code {\n"
     (constructor-code constructor yul-env sto-env)
     "    datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))\n" ; Copy runtime code.
     "    return(0, datasize(\"runtime\"))\n"
     "  }\n"

     ;; Runtime code.
     "  object \"runtime\" {\n"
     "    code {\n"         
     ;; Function dispatcher for external definitions.
     "      // Dispatcher\n"
     "      switch shr(224, calldataload(0))\n"
     (dispatcher-code (:external functions) yul-env)
     "      // Storage-access\n"
     (dispatcher-code (:external storage) yul-env)
     "      // External constants\n"         
     (dispatcher-code (:external constants) yul-env)
     "      default { revert(0,0) }\n\n"
     "\n"

     "      /* -------- external functions ---------- */\n"
     (functions-code (:external functions)
                     yul-env sto-env {:functions true})
     "      /* -------- internal functions ---------- */\n"
     (functions-code (:internal functions)
                     yul-env sto-env {:functions true})
     "\n"
     "      /* -------- storage access ---------- */\n"
     (functions-code (:external storage) yul-env '{} {:storage true})
     (functions-code (:internal storage) yul-env '{} {:storage true})
     "      /* -------- constants ---------- */\n"
     (functions-code (:external constants) yul-env '{} {:constants true})
     (functions-code (:internal constants) yul-env '{} {:constants true})
     "      /* -------- events ---------- */\n"
     (functions-code events yul-env '{} {:events true})
     "\n    }\n"
     "  }\n"
     "}")))

(defn symtable-to-yul
  [symbols yul-env sto-ns]
  (compile-to-yul
   symbols
   (add-funcs-to-env symbols yul-env)
   (init-storage-env symbols sto-ns)))
