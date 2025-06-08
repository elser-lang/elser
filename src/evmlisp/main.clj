(ns evmlisp.main
  (:gen-class)
  (:require [evmlisp.env :as env]
            [evmlisp.reader :as reader]
            [evmlisp.printer :as printer]
            [evmlisp.errors :as errs]
            [evmlisp.core :as core]
            [evmlisp.symtable :as symtable]
            [evmlisp.compiler :as compiler]
            [evmlisp.cli :as cli]
            [clojure.repl :as clj-repl]
            [clojure.pprint :refer [pprint]]))

(def prompt (fn [] (println "user> ") (flush)))

(defn READ
  [inp]
  (reader/read-str inp))

(declare EVAL)

(defn eval-ast
  [ast renv]
  (cond
    (symbol? ast) (env/eget renv ast)
    
    (map? ast) (let [k (keys ast)
                     v (doall (map (fn [x] (EVAL x renv)) (vals ast)))]
                 (zipmap k v))
    
    (seq? ast) (doall (map (fn [x] (EVAL x renv)) ast))
    
    (vector? ast) (vec (doall
                        (map (fn [x] (EVAL x renv)) ast)))
    
    :else ast))

(defn EVAL
  [ast renv]
  (cond
    (nil? ast) ast
    
    (not (seq? ast)) (eval-ast ast renv)
    
    (empty? ast) '()
    
    (seq? ast) (let [f (first ast)]
                 (cond

                   ;; 'def' - call the set method of the current environment
                   ;; (second parameter of EVAL called env) using the
                   ;; unevaluated first parameter (second list element)
                   ;; as the symbol key and the evaluated
                   ;; second parameter as the value.
                   (= f 'def!)
                   (let [k (second ast)
                         v (EVAL (nth ast 2) renv)]
                     (env/eset renv k v))                   

                   ;; 'let' - create a new environment using the
                   ;; current environment as the outer value and
                   ;; then use the first parameter as a list of
                   ;; new bindings in the "let*" environment.
                   (= f 'let*)
                   (let [let-env (env/env renv)]
                     (doseq [[b e] (partition 2 (first (rest ast)))]
                       (env/eset let-env b (EVAL e let-env)))
                     (EVAL (nth ast 2) let-env))

                   ;; 'do' - evaluate all the elements of the list
                   ;; and return the final evaluated element.
                   (= f 'do)
                   (last (eval-ast (rest ast) renv))

                   ;; 'if' - good old if statement.
                   (= f 'if)
                   (let [exprs (rest (rest ast))]
                     (if (EVAL (second ast) renv)
                       (EVAL (first exprs) renv)

                       (if (= (count exprs) 2)
                         (EVAL (second exprs) renv)
                         nil)))

                   ;; 'fn' - return a new function closure.
                   (= f 'fn*)
                   (let [p (second ast)
                         e (nth ast 2)]
                     (fn [& args]
                       (EVAL e (env/env renv p (or args '() )))))
                   
                   :else
                   (let [l' (eval-ast ast renv)
                         f (first l')
                         args (rest l')]
                     (apply f args))))))

(defn PRINT
  [inp]
  (printer/print-str inp true))

(def repl-env (env/env))
(defn rep
  [inp]
     (EVAL
      (READ inp) repl-env))

;; TODO: remove this Lisp init.
(doseq [[k v] core/core-ns] (env/eset repl-env k v))
(env/eset repl-env 'eval (fn [ast] (EVAL ast repl-env)))

(def yul-env (env/env))
(doseq [[k v] core/yul-ns] (env/eset yul-env k v))

;; Functions defined inside the language itself.
;;(rep "(def! not (fn* (x) (if x false true)))")
;; TODO: move 'load-file' into `EVAL` step?
; (rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

(defn repl-loop []
  (prompt)
  (let [line (read-line)]
    ; Skip comments
    (if (not= \; (get line 0))
      (try
        (println (rep line))
        (catch Throwable e (clj-repl/pst e))))
      (recur)))

(defn process-file [file options]
  (let [code (slurp file)
        ast (READ (str "(do " code "\nnil)"))]
    (cond
      (:ast options) 
      (do (println "Generated AST:")
          (clojure.pprint/pprint ast))

      (:ir options)
      (let [symbols (symtable/collect-symbols ast)]
        (println "Generated symbol-table:")
        (clojure.pprint/pprint symbols))

      (:yul options)
      (let [symbols (symtable/collect-symbols ast)
            yul (compiler/symtable-to-yul symbols yul-env)]
        (println "Generated Yul:")
        (println yul)))))
  
(defn -main [& args]
  (let [{:keys [file options exit-message ok?]} (cli/validate-args args)]
    (when exit-message
      (cli/exit (if ok? 0 1) exit-message))
    
    (if file      
      (process-file file options)
      (do (println "Starting REPL...")
          (repl-loop)))))
