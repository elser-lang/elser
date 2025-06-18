(defproject elser "0.1.0-SNAPSHOT"
  :description "Elser - explicit and structure-enforcing language for EVM."
  
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.cli "1.1.230"]
                 [org.web3j/core "4.13.0"]]
  
  :target-path "target/%s"

  :main elser.main)
