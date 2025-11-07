(ns elser.main
  (:gen-class)
  (:require [elser.env :as env]
            [elser.reader :as reader]
            [elser.printer :as printer]
            [elser.errors :as errs]
            [elser.core :as core]
            [elser.symtable :as symtable]
            [elser.typecheck :as typecheck]
            [elser.compiler :as compiler]
            [elser.evmcodegen :as evmcodegen]            
            [elser.cli :as cli]
            [elser.constants :as const]
            [clojure.repl :as clj-repl]))

;;; --------------------------------- Initializing Environments ---------------------------------

(def yul-env (env/env))
(doseq [[k v] core/yul-ns] (env/eset yul-env k v))

(def types-env (env/env))
(doseq [[k v] core/types-ns] (env/eset types-env k v))

;;; --------------------------------- REPL ---------------------------------

(defn READ [inp src] (reader/read-str inp src))

(defn read-eval-print
  [inp]
  (let [ast (READ inp "./")]
    (symtable/collect-symbols `(constructor ~ast))))

(defn repl-loop []
  (const/elser-prompt)

  (let [line (read-line)]

    (if (not= const/elser-comment (get line 0))
      (try
        (println (read-eval-print line))           
        (catch Throwable e (clj-repl/pst e))))
    
    (recur)))

(defn process-file
  "
  Apply compilation phases (to the content of a FILE)
  that correspond to provided OPTIONS.
  "
  [file options]
  (let [code (slurp file)
        ast (READ (str "(" code ")") file)]
    (cond
      (:ast options)
      (printer/pretty-print-phase "AST" ast)
      
      (:symtable options)
      (printer/pretty-print-phase "SYMBOL TABLE" (symtable/collect-symbols ast))

      (:yul options)
      (printer/print-phase "YUL"
                           (-> (symtable/collect-symbols ast)
                               (compiler/symtable-to-yul yul-env core/sto-ns)))


      (:compile options)
      (let [symbols (symtable/collect-symbols ast)
            _ (typecheck/check-types symbols types-env)]
        
        (printer/print-phase
         
         "EVM BYTECODE" (-> symbols
                            (compiler/symtable-to-yul yul-env core/sto-ns)
                            (evmcodegen/compile-to-evm (:ns symbols) (:pragma symbols))
                            )))))
  
  (cli/exit 0))
  
(defn -main [& args]
  (let [{:keys [file options exit-message ok?]} (cli/extract-cli-args args)]

    (when exit-message
      (cli/exit (if ok? 0 1) exit-message))
    
    (if file      
      (process-file file options)
      
      (do (println "Elser REPL")
          (repl-loop)))))
