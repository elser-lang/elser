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
