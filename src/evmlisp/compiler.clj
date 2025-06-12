(ns evmlisp.compiler
  (:gen-class)
  (:require [evmlisp.env :as env]
            [evmlisp.symtable :as symtable]
            [evmlisp.errors :as errs]
            [evmlisp.env :as env]            
            [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))

(def return-var "ret_val")

(defn add-funcs-to-env
  "
  Add function name => function call to the main env.
  "
  [symbols yul-env]
  (let [env-w-fns (env/env yul-env)
        funcs (into (:external (:functions symbols))
                    (:internal (:functions symbols)))
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
                    [] (:args definition))
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
                                 [(:name x) {:args (:args x) :slot (:slot x)}])) [] sto)
        ]
    (do
      ;; Set core storage operations.
      (doseq [[k v] sto-defs] (env/eset sto-env k v))
      (doseq [[k v] sto-ns] (env/eset sto-env k v)))
    sto-env))

(declare compile)

(defn compile-symbols
  [symbols yul-env sto-env returns?]
  (println "symbols" symbols)
  (cond
    (symbol? symbols) (env/eget yul-env symbols)
    
    (map? symbols) (let [k (keys symbols)
                         v (doall (map (fn [x] (compile x yul-env sto-env returns?))
                                       (vals symbols)))]
                     (zipmap k v))
    
    (seq? symbols) (mapv (fn [x] (compile x yul-env sto-env returns?))
                         symbols)
    
    (vector? symbols) (mapv (fn [x] (compile x yul-env sto-env returns?))
                            symbols)
    
    :else symbols))

;; TODO: finish 'return' statement!!!!
(defn compile
  [symbols yul-env sto-env returns?]
  (cond
    ;; TODO: what to do wtih this case?
    (nil? symbols) symbols
    
    (not (seq? symbols)) (compile-symbols symbols yul-env sto-env returns?)
    
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
                                                       returns?))))
                                             [] local-defs))]
                         (doseq [[b _] local-defs]
                           (env/eset let-env b b))                         
                         (str yul-lets "\n"
                              (compile (nth symbols 2) let-env sto-env returns?)))

                       ;; 'do' - evaluate all the elements of the list
                       ;; and return the final evaluated element.
                       (= f 'do)
                       (let [exprs (mapv
                                    (fn [sym] (compile
                                              sym
                                              yul-env
                                              sto-env
                                              returns?))
                                    (rest symbols))
                             l (- (count exprs) 1)
                             last-expr (get exprs l)]
                         (string/join "\n"
                                      (if returns?
                                        (assoc exprs l
                                               (format "%s := %s"
                                                       return-var last-expr))
                                        exprs)))

                       ;; 'loop' in Yul (loop [binds] (cond) (post-iter) (body))
                       ;; in evmlisp (loop [binds] (cond) (body) (post-iter)).
                       (= f 'loop)
                       (let [loop-env (env/env yul-env)
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
                                                       loop-env
                                                       sto-env
                                                       returns?))))
                                             [] local-defs))
                             [_ _ cnd body post-iter] symbols]
                         (doseq [[b _] local-defs]
                           (env/eset loop-env b b))
                         
                         (format "for { %s } %s { %s }\n { %s }\n"
                                 yul-lets
                                 (compile cnd loop-env sto-env returns?)
                                 (compile post-iter loop-env sto-env returns?)
                                 (compile body loop-env sto-env returns?)))                       

                       ;; 'sto' - a storage-access function.
                       ;; TODO: sstore(...) should use slot
                       ;; not variable name!
                       (= f 'sto)
                       (let [op (second symbols)
                             sto-var (nth symbols 2)
                             var-slot (:slot (env/eget sto-env sto-var))]
                         (cond
                           (= op 'write!)
                           (apply (env/eget sto-env op)
                                  [(:slot (env/eget sto-env sto-var))
                                   (compile (last symbols)
                                            yul-env sto-env returns?)])

                           (= op 'read!)
                           (apply (env/eget sto-env op) [sto-var])

                           :else
                           (errs/err-bind-not-found op)))
                       
                       :else
                       (let [l' (compile-symbols symbols yul-env sto-env returns?)
                             f (first l')
                             args (rest l')]
                         (apply f args))))))

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
  (format "          %s := sload(%s)\n"
          return-var (:slot definition)))

(defn compile-function-body
  [definition yul-env sto-env]
  (compile (:body definition)
           yul-env
           sto-env
           (not (empty? (:return definition)))))

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
      (format " -> %s " return-var)

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
    (compile-to-yul
     symbols
     (add-funcs-to-env symbols yul-env)
     (init-storage-env symbols sto-ns)))
