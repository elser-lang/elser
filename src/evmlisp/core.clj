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
   ['>= (fn [x y] (format "iszero(lt(%s, %s))" x y))]
   ['< (fn [x y] (format "lt(%s, %s)" x y))]
   ['<= (fn [x y] (format "iszero(gt(%s, %s))" x y))]
   
   ;; Logical operators (in progress)
   ['and (fn [x y] (format "and(%s, %s)" x y))]
   ['or (fn [e o] (format "or(%s, %s)" e o))]
   ['not (fn [e] (format "not(%s)" e))]

   ['caller (fn [] "caller()")]
   ['origin (fn [] "origin()")]

   ['assert (fn [c] (format "if iszero(%s) { revert(0,0) }\n" c))]
   ;; fix: messages aren't displayed yet.
   ['revert (fn [msg] "revert(0,0)\n")]

   ;; Control-flow statements
   ['if (fn [pred true-body false-body]
          (format
           "switch %s\n case 1 { %s }\n default { %s }"
           pred true-body false-body))]
   
   ['set! (fn [bind expr] (format "%s := %s" bind expr))]
   ['-> (fn [top local] (format "%s := %s" top local))]
   ])

(def sto-ns
  [
   ['read! (fn [var-name & args]
             (format "%s(%s)" var-name
                     (if (nil? args)
                       ""
                       args)))]
   
   ['write! (fn [slot val] (format "sstore(%s, %s)" slot val))]
   ])
