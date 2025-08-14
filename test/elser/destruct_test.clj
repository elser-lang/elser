(ns elser.destruct-test
  (:require [clojure.test :refer :all]
            [elser.main :refer :all]
            [elser.destruct :refer :all]))

(deftest test-destruct-def
  (testing "Destructure (const | storage | events) definitions"

    (let [c (destruct-def '(def y (-> (map :u256 (map :bool :u256))) {:slot 66}))]
      (is (= (:name c) 'y))
      (is (= (:args c) [:u256 :bool]))
      (is (= (:ret c) '({:name "elser_ret_val" :type :u256})))
      (is (= (:opts c) {:slot 66}))
      )

    (let [c (destruct-def '(def x (->
                                   (map :u256
                                        (map :u256
                                             (map :bool
                                                  (map :u256 :addr)))))))]
      (is (= (:name c) 'x))
      (is (= (:args c) [:u256 :u256 :bool :u256]))
      (is (= (:ret c) '({:name "elser_ret_val" :type :addr})))
      )    

    (let [c (destruct-def '(def X (-> (:u256)) 228))]
      (is (= (:name c) 'X))
      (is (= (:args c) []))
      (is (= (:ret c) '({:name "elser_ret_val" :type :u256})))
      (is (= (:opts c) 228))
      )

    (let [c (destruct-def '(def X (-> (:u256))))]
      (is (= (:name c) 'X))
      (is (= (:args c) []))
      (is (= (:ret c) '({:name "elser_ret_val" :type :u256})))
      (is (= (:opts c) nil))
      )

    (let [c (destruct-def '(def custom_slot (-> (:bool)) {:slot 1337}))]
      (is (= (:name c) 'custom_slot))
      (is (= (:args c) []))
      (is (= (:ret c) '({:name "elser_ret_val" :type :bool})))
      (is (= (:opts c) {:slot 1337}))
      )
    ))
