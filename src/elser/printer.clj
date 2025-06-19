(ns elser.printer
  (:gen-class)
  (:use [clojure.string :as string]))

(defn esc [s]
  (-> s (string/replace "\\" "\\\\")
        (string/replace "\"" "\\\"")
        (string/replace "\n" "\\n")))

(defn print-str
  "Takes generated AST and prints it as a string."
  ([ast] (print-str ast true))
  ([ast r?] 
  (cond
    (string? ast) (if r? (str "\"" (esc ast) "\"") ast)
    (fn? ast) (str "#<fn>" ast)
    (list? ast) (str "(" (string/join " " ast) ")")
    (nil? ast) "nil"
    :else (str ast))))
