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

(defn err-nil-symbol [sym]
  (throw (Exception. (format "evmlisp: nil symbol %s" sym))))

(defn err-unsupported-function-def [got want]
  (throw (Exception.
          (format "evmlisp: unsupported function defition: got [%s] | want [%s]"
                  got want))))

(defn err-slot-collision
  [s]
  (throw (Exception. (format "evmlisp: slot collision in slot %s" s))))


(defn err-gt1-return
  []
  (throw (Exception. "evmlisp: multi-return is unsupported.")))

(defn err-invalid-top-level-form [s]
  (throw (Exception. (format "evmlisp: top-level form must be a list (): %s" s))))

(defn err-invalid-nested-type [in-form have want]
  (throw (Exception.
          (format "evmlisp: invalid nested-type in form (%s): have %s | want %s"
                  in-form have want))))

(defn err-arity-exception [f have want]
  (throw (Exception.
          (format "evmlisp: invalid arity for function %s: have %s | want %s"
                  f have want))))


