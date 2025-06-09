(ns evmlisp.cli
  (:gen-class)
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string]))

(def cli-options
  [["-c" "--compile" "Compile to EVM bytecode."]
   ["-a" "--ast" "Show the AST."]
   ["-y" "--yul" "Show the compiled Yul code."]
   ["-s" "--symtable" "Show symbol-table generated from evmlisp."]
   ["-o" "--output FILE" "Output file for compilation results"
    :default "out.yul"]
   ["-h" "--help" "Show help message."]])

(defn usage [options-summary]
  (->> ["Usage: program [options] <file>"
        ""
        "Options:"
        options-summary
        ""
        "If no file is provided, starts a REPL"]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit and show usage summary.
      {:exit-message (usage summary) :ok? true}

      errors ; errors => exit with description of errors.
      {:exit-message (error-msg errors)}

      ;; Check if file exists.
      (and (seq arguments) (not (.exists (java.io.File. (first arguments)))))
      {:exit-message (str "File not found: " (first arguments))}

      :else ; everything is valid!
      {:file (first arguments)
       :options options
       :summary summary})))

(defn exit [status msg]
  (println msg)
  (System/exit status))
