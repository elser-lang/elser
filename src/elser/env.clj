(ns elser.env
  (:gen-class)
  (:require [elser.errors :as errs]))

(defn env [& [outer binds exp]]
  (atom (loop [env {:outer outer}
               b binds
               e exp]
          (cond
            (nil? b) env
       
            ;; Handle variadic functions.
            (= (first b) '&) (assoc env (first (next b)) (apply list e))         
         
            :else
            (recur (assoc env (first b) (first e)) (next b) (rest e))))))

(defn eset [env k v] (swap! env assoc k v) v)

(defn efind
  [env sym]
  (loop [e env]
    (cond
      (contains? @e sym) (@e sym)
      (nil? (:outer @e)) nil
      :else
      (recur (:outer @e)))))

(defn eget
  [env sym]
  (let [e (efind env sym)]
    (cond
      (nil? e) (errs/err-bind-not-found sym)
      
      :else e)))
