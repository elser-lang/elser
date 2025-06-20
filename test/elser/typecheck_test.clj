(ns elser.typecheck-test
  (:require [clojure.test :refer :all]
            [elser.main :refer :all]
            [elser.core :refer :all]
            [elser.env :refer :all]
            [elser.types :refer :all]
            [elser.typecheck :refer :all]))

(def test-prog "./test/elser/test-progs/valid_top_level.evml")
(def test-func {:name 'func
                :return '({:name c, :type :u256, :mutable? mut})
                :permissions {:w 1 :r 1}
                :args '({:name x, :type :u256, :mutable? mut})})
(def test-func-top {:functions {:external [test-func]}})
(def test-stovar {:name 'count,
                  :selector "0x06661abd",
                  :signature "count()",
                  :args '(),
                  :slot 0,
                  :return '({:name x, :type :u256, :mutable? mut}),
                  :var-type {:simple true}})
(def test-stovar-top {:storage {:external [test-stovar]}})

(def tenv (env))
(doseq [[k v] types-ns] (eset tenv k v))

(deftest infer-type-test
  (testing "Test if types are correctly inferred."
    (is (= (infer-type 12) :u256))
    (is (= (infer-type -12) :i256))
    (is (= (infer-type true) :bool))
    (is (= (infer-type false) :bool))
    (is (= (infer-type "0xd8da6bf26964af9d7eed9e03e53415d37aa96045") :addr))))

(deftest infer-invalid-type-test
  (testing "Test if invalid types are correctly caught."
    (is (thrown? Exception (infer-type "hey!"))) ;; no strings
    ;; Sequences shouldn't end-up in this function.
    (is (thrown? Exception (infer-type '[1])))
    (is (thrown? Exception (infer-type '(1))))
    (is (thrown? Exception (infer-type "0xd8bf26964af9d7eed9e03e53415d37aa96045")))))

(deftest type-check-test
  (testing "Test basic type-checking."
    (is (thrown? Exception (type-check {:type nil})))
    (is (= (type-check 12) {:type :u256}))
    (is (= (type-check {:type :u256}) {:type :u256}))
    (is (= (type-check {:type :i256}) {:type :i256}))
    (is (= (type-check {:type :bool}) {:type :bool}))
    (is (= (type-check {:type :addr}) {:type :addr}))))

(deftest type-check-op-test
  (testing "Test [T] against [t]."
    (is (= (type-check-op [:u256] [{:type :u256}]) [:u256]))
    (is (= (type-check-op [:i256] [{:type :i256}]) [:i256]))
    (is (= (type-check-op [:u256 :i256] [{:type :i256}]) [:u256 :i256]))
    (is (= (type-check-op [:bool] [{:type :bool}]) [:bool]))

    (is (= (type-check-op all [{:type :bool}]) all))
    
    (is (thrown? Exception (type-check-op [:bool] [{:type :i256}])))
    (is (thrown? Exception (type-check-op [:bool] [{:type :addr}])))
    ))

(deftest type-check-numeric-test
  (testing "Test tc for numeric ops."
    (is (= (type-check-numeric 12 13 nil) {:type :u256}))
    (is (= (type-check-numeric {:type :u256} 13 nil) {:type :u256}))
    (is (= (type-check-numeric {:type :u256} {:type :u256} nil) {:type :u256}))
    (is (= (type-check-numeric {:type :i256} {:type :u256} nil) {:type :i256}))
    (is (= (type-check-numeric {:type :i256} {:type :u256} nil) {:type :i256}))
    
    (is (thrown? Exception (type-check-numeric {:type :i256} {:type nil} nil)))
    (is (thrown? Exception (type-check-numeric {:type :i256} {:type nil} nil)))
    
    (is (= (type-check-numeric-unary {:type :u256}) {:type :u256}))
    (is (= (type-check-numeric-unary {:type :i256}) {:type :i256}))
    ))


(deftest type-check-bool-test
  (testing "Test tc for logical ops."
    (is (= (type-check-bool true false {:type :bool}) {:type :bool}))
    (is (= (type-check-bool false false {:type :bool}) {:type :bool}))

    (is (thrown? Exception (type-check-bool 1 false {:type :bool})))
    (is (thrown? Exception (type-check-bool 1 "xy" {:type :bool})))    
    ))

(deftest type-check-all-test
  (testing "Test tc against all."
    (is (= (type-check-all true false {:type :bool}) {:type :bool}))
    (is (= (type-check-all 5 16 {:type :bool}) {:type :bool}))

    (is (thrown? Exception (type-check-all "x" 16 {:type :bool}) {:type :bool}))
    ))


(deftest type-check-assign-test
  (testing "Test tc for assigning."
    ;; Assignement workf for mutables only!
    (is (= (type-check-assign {:type :u256 :mutable? 'mut} {:type :u256}) {:type :u256}))

    ;; Can't assign to immutables.
    (is (thrown? Exception (type-check-assign {:type :u256} {:type :u256})))
    ;; Can't assign to different type.
    (is (thrown? Exception (type-check-assign {:type :bool :mutable? 'mut}
                                              {:type :u256})))
    ))

(deftest type-check-eval-test
  (testing "Test eval-typechecking."
    
    ;; Result will be "mutable" because we use literals.
    (is (= (typecheck '(+ 2 2) tenv '{}) {:type :u256 :mutable? 'mut}))

    (is (= (typecheck '(+ 2 (* 2 2)) tenv '{}) {:type :u256 :mutable? 'mut}))

    ;; let.
    (is (= (typecheck '(let [x 2] (* x x)) tenv '{}) nil))

    ;; loop + mutations.
    (is (= (typecheck '(loop [x 10]
                         (while (> x 0) (* x x)
                                (set! x (- x 1)))) tenv '{}) nil))

    ;; do.
    (is (= (typecheck '(do (+ 2 2) (- 10 5)) tenv '{}) nil))

    ;; Function is evaluated to its return type.
    (is (= (:type
            (typecheck
             'func
             (init-local-types-env
              (add-types-to-env test-func-top tenv) test-func) '{}))
           (first (:return test-func))))

    ;; invoke!
    (is (= (typecheck
            '(invoke! func 15) ; program
            (init-local-types-env
             (add-types-to-env test-func-top tenv) test-func)
            {:w 1 :r 1})
           (first (:return test-func))))

    ;; sto [read!]
    (is (= (typecheck
            '(sto read! count) ; program
            (init-local-types-env
             (add-types-to-env test-stovar-top tenv) test-stovar)
            {:w 0 :r 1}) ; allow writing.
           (first (:return test-stovar))))

    ;; sto [write!]
    (is (= (typecheck
            '(sto write! count (+ (sto read! count) 22)) ; program
            (init-local-types-env
             (add-types-to-env test-stovar-top tenv) test-stovar)
            {:w 1 :r 1}) ; allow writing.
           (first (:return test-stovar))))
    
    ;; no reading permission.
    (is (thrown? Exception (typecheck
                            '(sto read! func) ; program
                            (init-local-types-env tenv test-stovar)
                            {:w 1 :r 0})))
    ))
