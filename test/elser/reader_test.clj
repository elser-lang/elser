(ns elser.reader-test
  (:require [clojure.test :refer :all]
            [elser.main :refer :all]
            [elser.reader :refer :all]))

;; All characters are wrapped in parentheses, because that's what's done
;; in the elser.main before input is sent to the tokenization function.
(def two-lines "(\n)")
(def five-lines "(\n\n\n\n)")
(def test-els "((ns test (:pragma \"0.8.29\"))

(constructor
 (sto write! x \"0xd8da6bf26964af9d7eed9e03e53415d37aa96045\"))

(events
 (
  (def xyz ((param0 :addr) (param1 :addr)))
  ))

(constants
 (:internal
  ( 
   (def CONST (-> (:addr)) \"0x0000000000000000000000000000000000000000\")   
   )))

(storage
 (:external
  (
   
   (def slot0 (-> (x mut :addr)))
   
   )))

(functions
 (:external
   
   (defn doStuff ((param1 mut :addr)) (@sto :w 1 :r 1) (-> ())
     (assert (!= param1 CONST))
     (invoke! function2 param1))
   
   (defn function2 ((parameter mut :addr)) (@sto :w 1 :r 1) (-> ())
     (sto write! slot0 parameter)
     (invoke! doStuff CONST))
   )
  ))")

(deftest test-tokenize-with-metadata-new-lines
  (testing "Tokenize the source with line & pos metadata.")

  (let [[tokens-2l ctx-2l] (tokenize-with-metadata two-lines)
        [tokens-5l ctx-5l] (tokenize-with-metadata five-lines)]

    ;; Verify "(\n)"
    (let [token-0 (get tokens-2l 0)
          token-1 (get tokens-2l 1)]
      
      (is (= (:char token-0) "("))
      (is (= (:line token-0) 1))

      (is (= (:char token-1) ")"))
      (is (= (:line token-1) 2))

      (is (= (get ctx-2l 1) "("))
      (is (= (get ctx-2l 2) "\n)"))      
      )

    ;; Verify four new lines
    (let [token-0 (get tokens-5l 0)
          token-1 (get tokens-5l 1)]
      
      (is (= (:char token-0) "("))
      (is (= (:line token-0) 1))

      (is (= (:char token-1) ")"))
      (is (= (:line token-1) 5))

      (is (= (get ctx-5l 1) "("))
      (is (= (get ctx-5l 5) "\n\n\n\n)"))      
      )
    ))

(deftest test-tokenize-with-metadata-ownable
  (testing "Tokens in program can be properly annotated with lines and pos metadata.")

  (let [[tokens ctx] (tokenize-with-metadata test-els)
        total-lines (:line (last tokens))]
    
    (is (= total-lines 36))
    ))
