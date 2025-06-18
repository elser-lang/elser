(ns elser.evmcodegen
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]))

(defn compile-to-evm
  [yul-code contract-name pragma]
  (let [_ (.mkdirs (io/file "out"))
        
        yul-path (str "out/" contract-name ".yul")
        _ (spit yul-path yul-code)
        
        solc-cmd ["solc" "--strict-assembly" "--optimize" 
                  "--optimize-runs=200" yul-path]
        
        {:keys [exit out err]} (apply sh/sh solc-cmd)]

    (if (zero? exit)
      (let [bytecode (->> (clojure.string/split out #"\n")
                          (drop-while #(not= % "Binary representation:"))
                          (drop 1)
                          (take-while #(not= % "Text representation:"))
                          (clojure.string/join "\n"))
            
            ;; Save artifacts (for the future).
            artifact {:bytecode bytecode}]
        (spit (str "out/" contract-name ".bytecode") bytecode))

        (throw (Exception.
                (format "solc compilation failed: %s\nsolc command: %s" err solc-cmd))))))
