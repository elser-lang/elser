(defproject evmlisp "0.1.0-SNAPSHOT"
  :description "EVMLISP - Lisp for EVM"
  
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.1.230"]
                 [org.web3j/core "4.13.0"]]
  
  :target-path "target/%s"

  :main evmlisp.main)
