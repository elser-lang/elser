(ns elser.evmcodegen
  (:gen-class)
  (:require [clojure.string :as string]
            [clojure.java.shell :as sh]
            [clojure.java.io :as io]))

(defn extract-code 
  "Extract code parts between START and END from stdout."
  [out start end]
  (->> (clojure.string/split out #"\n")
       (drop-while #(not= % start))
       (drop 1)
       (take-while #(not= % end))
       (clojure.string/join "\n")))

(defn compile-to-evm
  [yul-code contract-name pragma]
  (let [_ (.mkdirs (io/file "out"))
        
        yul-path (str "out/" contract-name ".yul")
        _ (spit yul-path yul-code)
        
        solc-cmd ["solc" "--strict-assembly" "--optimize" 
                  "--optimize-runs=200" yul-path]
        
        {:keys [exit out err]} (apply sh/sh solc-cmd)]

    (if (zero? exit)
      (let [bytecode (extract-code out "Binary representation:" 
                                   "Text representation:")
            yul-optimized (extract-code out "Pretty printed source:"
                                        "Binary representation:")
            
            ;; Save artifacts (for the future).
            artifact {:bytecode bytecode}]
        (spit (str "out/" contract-name ".bytecode") bytecode)
        (spit (str "out/" contract-name ".yul") yul-optimized))

      (throw (Exception.
              (format "solc compilation failed: %s\nsolc command: %s" err solc-cmd))))))
