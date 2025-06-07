(ns evmlisp.errors
  (:gen-class))

(defn err-eof-before-paren [] (throw (Exception. "evmlisp: EOF before ')'")))

(defn err-unexpected-tkn [s]
  (throw (Exception. (format "evmlisp: unexpected end of input: %s" s))))

(defn err-unbalanced
  [s]
  (throw (Exception. (format "evmlisp: unabalanced %s" s))))

(defn err-invalid-sym
  [s]
  (throw (Exception. (format "evmlisp: invalid symbol %s" s))))

(defn err-bind-not-found [sym]
  (throw (Exception. (format "evmlisp: bind not found %s" sym))))

(defn err-slot-collision
  [s]
  (throw (Exception. (format "evmlisp: slot collision in slot %s" s))))
