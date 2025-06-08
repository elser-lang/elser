(ns evmlisp.core
  (:gen-class)
  (:require [evmlisp.env :as env]
            [evmlisp.printer :as prnt]
            [evmlisp.reader :as reader]))

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
   ;; Arithemtic (in progress)
   ['+ "add(%s, %s)"]
   ['* "mul(%s, %s)"]
   ['- "sub(%s, %s)"]
   ['/ "div(%s, %s)"]
   ;['count ] todo: return lenght of data.
   ['mod "mod(%s, %s)"]
   ['** "exp(%s, %s)"]

   ;; Comparison (in progress)
   ['= "eq(%s, %s)"]
   ['> "gt(%s, %s)"]
   ['< "lt(%s, %s)"]
   ['<= "iszero(gt(%s, %s))"]

   ;; Logical operators (in progress)
   ['& "and(%s, %s)"]
   ['| "or(%s, %s)"]
   ['~ "not(%s)"]
   ['& "and(%s, %s)"]])
