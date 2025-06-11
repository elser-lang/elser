(ns evmlisp.core
  (:gen-class)
  (:require [evmlisp.env :as env]
            [evmlisp.printer :as prnt]
            [evmlisp.reader :as reader]))

;; TODO: remove
(def core-ns
  [['+ +]
   ['- -]
   ['* *]
   ['/ /]
   ['list list]
   ['list? list?]
   ['empty? empty?]
   ['count count]
   ['str str]
   ['read-string reader/read-str]
   ['prn (fn [ast] (prnt/print-str ast true))]
   ['slurp slurp]
   ['= =]
   ['> >]
   ['< <]
   ['>= >=]
   ['<= <=]])

(def yul-ns
  [
   ;; Arithemtic, binary operations.
   ['+ (fn [x y] (format "add(%s, %s)" x y))]
   ['* (fn [x y] (format "mul(%s, %s)" x y))]
   ['- (fn [x y] (format "sub(%s, %s)" x y))]
   ['/ (fn [x y] (format "div(%s, %s)" x y))]
   ['mod (fn [x y] (format "mod(%s, %s)" x y))]
   ['exp (fn [x y] (format "exp(%s, %s)" x y))]

   ;; Comparison (in progress)
   ['= (fn [x y] (format "eq(%s, %s)" x y))]
   ['> (fn [x y] (format "gt(%s, %s)" x y))]
   ['< (fn [x y] (format "lt(%s, %s)" x y))]
   ['<= (fn [x y] (format "iszero(gt(%s, %s))" x y))]
   
   ;; Logical operators (in progress)
   ['& "and(%s, %s)"]
   ['| "or(%s, %s)"]
   ['~ "not(%s)"]
   ['& "and(%s, %s)"]])

(def sto-ns
  [
   ['read! (fn [var-name & args]
             (format "%s(%s)" var-name
                     (if (nil? args)
                       ""
                       args)))]
   ['write! (fn [slot val] (format "sstore(%s, %s)" slot val))]
   ])
