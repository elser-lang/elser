(ns elser.symtable-test
  (:require [clojure.test :refer :all]
            [elser.main :refer :all]
            [elser.reader :refer :all]
            [elser.symtable :refer :all]))

(def test-ast
  (quote (
          (ns testProgram (:pragma "0.8.20"))
          (constructor (sto write! owner (caller)))
          (events (
                   (def OwnershipTransferred ((prev :addr) (new :addr)))
                   ))
          (constants 
           (
            :external 
            ((def ADDRESS_ZERO ((a :addr)) 0x0000000000000000000000000000000000000000))
            :internal 
            ((def A_ZERO ((a :addr)) 0x0000000000000000000000000000000000000000))            
            ))

          (storage (
                    :external (
                               (def owner ((o :addr)))
                               )
                    
                    :internal (
                               (def _owner_ ((o :addr)))
                               )))
          
;
          (functions (:external
                      (
                       (defn ownerfn0 ((o :addr)) (@sto :w 0 :r 1) (-> ((o mut :addr)))
                         (-> o (sto read! owner)))

                       )
                      :internal 
                      (
                       (defn ownerfn ((o :addr)) (@sto :w 0 :r 1) (-> ((o mut :addr)))
                         (-> o (sto read! ownerfn)))
                       
                       ))))
         
         ))

;; TODO:
(deftest top-level-collect-symbols
  (testing "Top level structure of the Elser code."
    (println (collect-symbols test-ast))))

(deftest test-extract-external-internal
  (testing "Can properly process :external and :internal defintions."

    (is (thrown? Exception 
                 (extract-external-internal
                  '(top-level-name (:bad () :wrong)))))

    ;; Can't define empty :external :internal keys.
    (is (thrown? Exception 
                 (extract-external-internal
                  '(top-level-name (:external :internal)))))

    (is (thrown? Exception 
                 (extract-external-internal
                  '(top-level-name (:external () :internal ())))))    

    (is (thrown? Exception 
                 (extract-external-internal
                  '(top-level-name (:external {} :internal {})))))

    (is (thrown? Exception 
                 (extract-external-internal
                  '(top-level-name (:external [] :internal [])))))

    (let [symbols (collect-symbols test-ast)
          storage (:storage symbols)]

      ;; external/internal definitions were converted to vector.
      (is (vector? (:external storage)))
      (is (vector? (:internal storage)))      
      )
    ))

(deftest test-process-storage
  (testing "Storage is correctly processed."

    ))

(deftest test-process-constructor
  (testing "Constructor form is correctly processed."
    (let [symbols (collect-symbols test-ast)
          constructor (:constructor symbols)
          body (:body constructor)]
      (is (list? body))
      (is (= '(sto write! owner (caller)) body))
      (is (= (count body) 4)))))
