(ns evmlisp.compiler
  (:gen-class)
  (:require [evmlisp.env :as env]
            [evmlisp.symtable :as symtable]
            [evmlisp.errors :as errs]
            [clojure.string :as string]))

(defn calldata-context [offset] {:offset (atom offset)})
(defn c-next-offset [ctx size] (swap! (:offset ctx) (fn [o] (+ o size))))

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

(declare evmlisp-args->yul-args)

(defn compile-calldata-args [definition]  
  (str "        mstore(0, " (:name definition)"("
       (loop [args-load "calldataload(4)"
              start-offset 4
              args-count (count (:args definition))]
         (println "args-load" args-load)
         (cond
           (= args-count 0) args-load
           :else (recur
                  (str args-load ","
                       (format "calldataload(%s)" start-offset))
                  (+ start-offset 32)  ; todo: need to use real size!
                  (- args-count 1)))) ")"))


(defn constructor-code [constructor yulenv] "    // Constructor is unsupported rn\n")

(defn get-dispatcher [definitions yulenv]
  (reduce (fn [dispatcher definition]
            (str dispatcher
                 (str
                  "      case " (:selector definition) " /* " (:signature definition) "*/ {\n"
                         (compile-calldata-args definition) ")\n"
                  "        return(0,0) // TODO: implement body\n"
                  "      }\n")))
          ""
          definitions))

(defn evmlisp-args->yul-args
  [args]
  (string/join
   ","  
   (reduce
    (fn [whole a]
      (conj whole
            (str (:name a)))) [] args)))

;; TODO: handle dynamics data types.
(defn compile-storage-var-body [definition yul-env]
    (format "          ret_val := sload(%s)\n" (:slot definition)))

(defn compile-function-body [definition yul-env]
  "          revert(0,0)\n")

(defn evmlisp-func-body->yul-func-body
  [definition yul-env]
  (let [storage? (contains? definition :slot)]
    (if storage?
      (compile-storage-var-body definition yul-env)
      (compile-function-body definition yul-env))))

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
  [definition yulenv]
  (str
   "      function " (:name definition) "("
   (evmlisp-args->yul-args (:args definition)) ")" (evmlisp-ret->yul-ret
                                                    (:return definition)) "{\n"
   (evmlisp-func-body->yul-func-body definition yulenv)
   "      }\n"))

;; TODO: utilzie other function-creation methods!
(defn storage-access
  "Generates storage access functions."
  [ex-sto yulenv]
  (str "      /* -------- storage access ---------- */\n"
       (reduce (fn [functions definition]
                 (str functions
                      (evmlisp-func->yul-func definition yulenv))) "" ex-sto)))

(defn create-functions
  [definitions yulenv]
  (reduce (fn [functions definition]
            (str functions
                 (evmlisp-func->yul-func definition yulenv)))
          ""
          definitions))


(defn generate-yul
  "This function is doing a template-based Yul code generation."
  [symbols yulenv]  
  (let [contract-name (:ns symbols)
        constructor (:constructor symbols)
        storage (:storage symbols)
        functions (:functions symbols)]
    (println "name" contract-name)
    (println "storage" storage)
    ;; TODO: add constructor code (if there's one).
    (str "object \"" contract-name "\" {\n"
         "  code {\n"
         (constructor-code constructor yulenv)
         "    datacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))\n"
         "    return(0, datasize(\"runtime\"))\n"
         "  }\n"
         "  object \"runtime\" {\n"
         "    code {\n"         
         ;;~~~~~~~  Dispatcher
         "      // Dispatcher\n"
         "      switch shr(224, calldataload(0))\n"
         (get-dispatcher (:external functions) yulenv)
         (get-dispatcher (:external storage) yulenv)
         "      default { revert(0,0) }\n\n"
         ;;~~~~~~~ External write functions
         "\n"
         (create-functions (:external functions) yulenv)
         "\n"
         ""
         (storage-access (:external storage) yulenv)
         "\n    }\n"
         "  }\n"
         "}")))


(defn symtable-to-yul
  [symbols yulenv]
  (generate-yul symbols yulenv))
