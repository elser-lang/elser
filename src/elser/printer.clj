(ns elser.printer
  (:gen-class)
  (:require [clojure.string :as string]
            [clansi]))

(defrecord ErrorMetadata [description chars path line pos])

(defn err-meta [description chars path line pos]
  (ErrorMetadata. description chars path line pos))

(defn fmt-err
  "Returns formatted error metadata."
  [metadata]
  (str
   "\n"
   (format "| Error: %s" (:description metadata)) "\n"
   (format "| >>> %s" (:path metadata)) "\n"
   (format "| L:%s %s" (:line metadata) (:chars metadata)) "\n"
   ))

(defn highlight [char] (clansi/style char :inverse :underline :red))

(defn highlight-char-in-ctx
  [ctx char]
  (string/replace ctx char (highlight char)))

(defn esc [s]
  (-> s (string/replace "\\" "\\\\")
        (string/replace "\"" "\\\"")
        (string/replace "\n" "\\n")))

(defn print-string
  "Takes generated AST and prints it as a string."
  ([ast] (print-str ast true))
  ([ast r?] 
  (cond
    (string? ast) (if r? (str "\"" (esc ast) "\"") ast)
    (fn? ast) (str "#<fn>" ast)
    (list? ast) (str "(" (string/join " " ast) ")")
    (nil? ast) "nil"
    :else (str ast))))
