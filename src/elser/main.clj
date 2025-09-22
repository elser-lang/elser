(ns elser.main
  (:gen-class)
  (:require [elser.env :as env]
            [elser.reader :as reader]
            [elser.errors :as errs]
            [elser.core :as core]
            [elser.symtable :as symtable]
            [elser.typecheck :as typecheck]
            [elser.compiler :as compiler]
            [elser.evmcodegen :as evmcodegen]            
            [elser.cli :as cli]
            [clojure.repl :as clj-repl]
            [clojure.pprint :refer [pprint]]))

(def prompt (fn [] (print ">>> ") (flush)))

(def yul-env (env/env))
(doseq [[k v] core/yul-ns] (env/eset yul-env k v))

(def types-env (env/env))
(doseq [[k v] core/types-ns] (env/eset types-env k v))

(defn READ
  [inp src]
  (reader/read-str inp src))

(defn rep
  [inp]
  (let [ast (READ inp "./")]
    (symtable/collect-symbols `(constructor ~ast))))

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
        ast (READ (str "(" code ")") file)]
    (cond
      (:ast options) 
      (do (println "Generated AST:")
          (clojure.pprint/pprint ast))

      (:symtable options)
      (let [symbols (symtable/collect-symbols ast)]
        (println "Generated symbol-table:")
        (clojure.pprint/pprint symbols))

      (:yul options)
      (let [symbols (symtable/collect-symbols ast)
            yul (compiler/symtable-to-yul symbols yul-env core/sto-ns)]
        (println "Generated Yul:")
        (println yul))

      (:compile options)
      (let [symbols (symtable/collect-symbols ast)
            _ (typecheck/check-types symbols types-env)
            yul (compiler/symtable-to-yul symbols yul-env core/sto-ns)]
        (do (evmcodegen/compile-to-evm yul (:ns symbols) (:pragma symbols))
            (println "EVM bytecode was successfully generated.")))))
  (System/exit 0))
  
(defn -main [& args]
  (let [{:keys [file options exit-message ok?]} (cli/validate-args args)]
    (when exit-message
      (cli/exit (if ok? 0 1) exit-message))
    
    (if file      
      (process-file file options)
      
      (do (println "Elser REPL")
          (repl-loop)))))
