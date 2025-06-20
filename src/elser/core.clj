(ns elser.core
  (:gen-class)
  (:require [elser.env :as env]
            [elser.printer :as prnt]
            [elser.types :as types]            
            [elser.reader :as reader]))

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
   ['!= (fn [x y] (format "not(eq(%s, %s))" x y))]   
   ['> (fn [x y] (format "gt(%s, %s)" x y))]
   ['>= (fn [x y] (format "iszero(lt(%s, %s))" x y))]
   ['< (fn [x y] (format "lt(%s, %s)" x y))]
   ['<= (fn [x y] (format "iszero(gt(%s, %s))" x y))]
   
   ['and (fn [x y] (format "and(%s, %s)" x y))]
   ['or (fn [e o] (format "or(%s, %s)" e o))]
   ['not (fn [e] (format "not(%s)" e))]

   ['caller (fn [] "caller()")]
   ['callvalue (fn [] "callvalue()")]   
   ['origin (fn [] "origin()")]
   ['self (fn [] "address()")]
   ['balance (fn [a] (format "balance(%s)" a))]
   ['timestamp (fn [] "timestamp()")]

   ['assert (fn [c] (format "if iszero(%s) { revert(0,0) }\n" c))]
   ['require (fn [c msg]
               (format
                "if iszero(%s) { let err := \"%s\" mstore(0, err) revert(0,32) }\n"
                c msg))]
   
   ;; fix: messages aren't displayed yet.
   ['revert (fn [msg] (format "let msg := \"%s\" mstore(0,msg) revert(0,32)\n" msg))]
   ['emit! (fn [func args] (apply func args))]

   ;; Control-flow statements
   ['if (fn [pred true-body false-body]
          (format
           "switch %s\n case 1 { %s }\n default { %s }"
           pred true-body false-body))]
   
   ['set! (fn [bind expr] (format "%s := %s" bind expr))]
   ['invoke! (fn [func args] (apply func args))]
   ['-> (fn [top local] (format "%s := %s" top local))]
   ])

(def sto-ns
  [
   ['read! (fn [var-name & args]
             (format "%s(%s)" var-name
                     (if (nil? args)
                       ""
                       args)))]

   ;; TODO: handle arrays/maps.
   ['write! (fn [sto-var val]
              (format "sstore(%s, %s)" (:slot sto-var) val))]
   ])

(def types-ns
  [
   ['+ (fn [x y] (types/type-check-numeric x y nil))]
   ['* (fn [x y] (types/type-check-numeric x y nil))]
   ['- (fn [x y] (types/type-check-numeric x y nil))]
   ['/ (fn [x y] (types/type-check-numeric x y nil))]
   ['mod (fn [x y] (types/type-check-numeric x y nil))]
   ['exp (fn [x y] (types/type-check-numeric x y nil))]

   ['= (fn [x y] (types/type-check-all x y {:type :bool}))]
   ['!= (fn [x y] (types/type-check-all x y {:type :bool}))]
   ['> (fn [x y] (types/type-check-all x y {:type :bool}))]
   ['>= (fn [x y] (types/type-check-all x y {:type :bool}))]
   ['< (fn [x y] (types/type-check-all x y {:type :bool}))]
   ['<= (fn [x y] (types/type-check-all x y {:type :bool}))]
   
   ['and (fn [x y] (types/type-check-bool x y {:type :bool}))]
   ['or (fn [x y] (types/type-check-bool x y {:type :bool}))]
   ['not (fn [x] (types/type-check-bool-unary x {:type :bool}))]

   ['caller (fn [] {:type ':addr :mutable? nil})]
   ['callvalue (fn [] {:type ':u256 :mutable? nil})]
   ['origin (fn [] {:type ':addr :mutable? nil})]
   ['self (fn [] {:type ':addr :mutable? nil})]
   ['balance (fn [a] {:type ':u256 :mutable? nil})]
   ['timestamp (fn [] {:type ':u256 :mutable? nil})]

   ['assert (fn [x] (types/type-check-bool-unary x {:type :bool}))]
   ['require (fn [c msg] (types/type-check-bool-unary c {:type :bool}))]
   
   ['revert (fn [msg] {:type nil})]
   ['emit! (fn [func & args] nil)]
   ['transfer* (fn [to val] (do (types/type-check-addr to)
                               (types/type-check-numeric-unary val)))]

   ;; TODO: doesn't check true/false branches.
   ['if (fn [pred true-body false-body]
          (types/type-check-bool-unary pred {:type :bool}))]
   
   ['set! (fn [bind expr] (types/type-check-assign bind expr))]
   ['invoke! (fn [func args] (types/type-check-op
                             (mapv conj (:args func))
                             (mapv conj args)
                             ))]
   ['-> (fn [top local] (types/type-check-assign top local))]
   ])
