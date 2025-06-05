(ns evmlisp.errors
  (:gen-class))

(defn err-eof-before-paren [] (throw (Exception. "evmlisp: EOF before ')'")))

(defn err-unexpected-tkn [s]
  (throw (Exception. (str "evmlisp: unexpected end of input: " s))))

(defn err-unbalanced
  [s]
  (throw (Exception. (str "evmlisp: unabalanced" s))))

(defn err-invalid-sym
  [s]
  (throw (Exception. (str "evmlisp: invalid symbol " "'" s "'"))))

(defn err-bind-not-found [sym]
  (throw (Exception. (str "evmlisp: bind not found " "'" sym "'"))))
