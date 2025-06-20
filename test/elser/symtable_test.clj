(ns elser.symtable-test
  (:require [clojure.test :refer :all]
            [elser.main :refer :all]
            [elser.symtable :refer :all]))

(def valid-top-level "./test/elser/test-progs/valid_top_level.evml")
(defn invalid-top-level [i]
  (format "./test/elser/test-progs/invalid_top_level%s.evml" i))

(deftest top-level-collect-symbols
  (testing "Top level elser lists."
    (let [code (slurp valid-top-level)
          ast (READ (str "(" code ")"))
          symbols (collect-symbols ast)]
      (is (= (:ns symbols) 'empty))
      (is (contains? symbols :constructor))
      (is (contains? symbols :events))
      (is (contains? symbols :constants))
      (is (contains? symbols :storage))
      (is (contains? symbols :functions))
      )))
