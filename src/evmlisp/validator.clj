;; TODO: this namespace should implement some compile-time validations.
(ns evmlisp.validator
  (:gen-class))

(defn validate-slots
  "
  Validates storage-slots collision at compile-time.
  Mainly useful for custom slot assignement via {:slot 0xPOS}.
  "
  [defs]
  (let [used-slots (map :slot defs)]
    (assert (= (count used-slots) (count (distinct used-slots)))
            "Storage slot collision detected")))
