(ns elser.errors
  (:gen-class)
  (:require [elser.printer :as printer]))

(defn err-eof-before-paren [] (throw (Exception. "elser: EOF before ')'")))

(defn err-throw [metadata]
  (throw (Exception. (printer/fmt-err metadata))))

(defn err-unexpected-tkn [s]
  (throw (Exception. (format "elser: unexpected token: %s" s))))

(defn err-unbalanced
  [s]
  (throw (Exception. (format "elser: unabalanced %s" s))))

(defn err-invalid-sym
  [s]
  (throw (Exception. (format "elser: invalid symbol %s" s))))

(defn err-bind-not-found [sym]
  (throw (Exception. (format "elser: bind not found %s" sym))))

(defn err-nil-symbol [sym]
  (throw (Exception. (format "elser: nil symbol %s" sym))))

(defn err-unsupported-function-def [got want]
  (throw (Exception.
          (format "elser: unsupported function defition: got [%s] | want [%s]"
                  got want))))

(defn err-slot-collision
  [s]
  (throw (Exception. (format "elser: slot collision in slot %s" s))))


(defn err-invalid-top-level-form [s]
  (throw (Exception. (format "elser: top-level form must be a list (): %s" s))))

(defn err-top-level-already-defined [s]
  (throw (Exception. (format "elser: top-level form already defined: %s" s))))

(defn err-invalid-ex-in-definition [s]
  (throw (Exception. (format "elser: invalid :external :internal definition: %s" s))))

(defn err-invalid-nested-type [in-form have want]
  (throw (Exception.
          (format "elser: invalid nested-type in form (%s): have %s | want %s"
                  in-form have want))))

(defn err-invalid-nested-constr-form [have want]
  (throw (Exception.
          (format "elser: invalid nested form in (constructor): have %s | want %s"
                  have want))))

(defn err-arity-exception [f have want]
  (throw (Exception.
          (format "elser: invalid arity for function %s: have %s | want %s"
                  f have want))))


(defn err-non-upper-case-const
  [s]
  (throw (Exception. (format "elser: constant name must be in upper case %s" s))))

(defn err-incorrect-loop-def
  []
  (throw (Exception.
          (format "elser: invalid loop defintion, should be (loop [binds] (cond) (body) (post-iter))"))))

(defn err-incorrect-loop-part
  [h w]
  (throw (Exception.
          (format "elser: invalid part of the loop: have %s | want %s" h w))))


(defn err-incorrect-return-symbol
  [s]
  (throw (Exception.
          (format "elser: return symbol should be '->', got %s" s))))

(defn err-incorrect-arr-def
  [have]
  (throw (Exception.
          (format
           "elser: invalid array defintion: have (def name [(%s :type)]) | want (def name [(array :type)])"))))

(defn err-bad-type [t]
  (throw (Exception. (format "elser: bad type %s" t))))

(defn err-bad-type-op [T t]
  (throw (Exception. (format "elser: bad type for op: have %s | want: %s" t T))))

(defn err-unsupported-literal-type [s]
  (throw (Exception. (format "elser: unsupported literal type: %s" s))))

(defn err-set-on-immutable [t]
  (throw (Exception. (format "elser: set! for immutable: %s" t))))

(defn err-invalid-def-key [h w n]
  (throw (Exception. (format "elser: (%s) invalid definition: have %s | want %s" n h w))))

(defn err-invalid-permission-value [h w]
  (throw (Exception. (format "elser: invalid permission value: have %s | want %s" h w))))

(defn err-invalid-permissions [f h w]
  (throw (Exception. (format "elser: invalid permissions: fn %s | have %s | want %s"
                             f h w))))

(defn err-sto-access-non-int [a]
  (throw (Exception. (format "elser: invalid type for @sto access value: %s" a))))

(defn err-diff-types-comp [x y]
  (throw (Exception. (format "elser: comparing diff types: x %s | y %s" x y))))
