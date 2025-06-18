(ns elser.typecheck
  (:gen-class)
  (:require [elser.errors :as errs]
            [elser.compiler :as compiler]
            [elser.types :as types]
            [clojure.string :as string]
            [elser.env :as env]))

(declare typecheck)
(defn typecheck-symbols
  [symbols types-env]
  (cond
    (symbol? symbols) (env/eget types-env symbols)
    
    (map? symbols) (let [k (keys symbols)
                         v (doall (map (fn [x] (typecheck x types-env))
                                       (vals symbols)))]
                     (zipmap k v))
    
    (seq? symbols) (mapv (fn [x] (typecheck x types-env))
                         symbols)
    
    (vector? symbols) (mapv (fn [x] (typecheck x types-env))
                            symbols)
    
    :else {:type (types/infer-type symbols) :mutable? 'mut}))

;; FIX: this is basically evaluation function (like `compile`).
;;      Thus, it's needed to utilize existing one.
(defn typecheck
  [symbols types-env]
  (cond
    ;; TODO: what to do wtih this case?
    (nil? symbols) symbols
    
    (not (seq? symbols)) (typecheck-symbols symbols types-env)
    
    (empty? symbols) '()
    
    (seq? symbols) (let [f (first symbols)]
                     (cond
                       (= f 'let)
                       (let [let-env (env/env types-env)
                             local-defs (partition 2 (first (rest symbols)))]
                         (doseq [[a b] local-defs]
                           (env/eset let-env a
                                     (assoc (typecheck b types-env)
                                            :mutable? 'mut)))
                         (mapv (fn [i] (typecheck (nth symbols i)
                                               let-env))
                               (range 2 (count symbols))))

                       (= f 'loop)
                       (let [loop-env (env/env types-env)
                             ;; Typecheck bindings like 'let'.
                             [_ _ cnd body post-iter] symbols
                             slt (assoc (vec symbols) 0 'let)]
                         (doseq [[a b] (partition 2 (first (rest symbols)))]
                           (env/eset loop-env a
                                     (assoc (typecheck b types-env)
                                            :mutable? 'mut)))
                         (do 
                           (typecheck (second cnd) loop-env)
                           (typecheck post-iter loop-env)
                           (typecheck body loop-env)))
                       
                       ;; 'do' - evaluate all the elements of the list
                       ;; and return the final evaluated element.
                       (= f 'do)
                       (let [exprs (mapv
                                    (fn [sym]
                                      (typecheck
                                       sym
                                       types-env))
                                    (rest symbols))
                             l (- (count exprs) 1)
                             last-expr (get exprs l)])

                       (= f 'invoke!)
                       (:type
                        (first
                         (:return (typecheck (first (rest symbols)) types-env))))

                       (= f 'sto)
                       (let [op (second symbols)
                             sto-var (nth symbols 2)]
                         (cond
                           (= op 'write!)
                           (typecheck (last symbols) types-env)

                           (= op 'read!)
                           (first
                            (:return (typecheck (last symbols) types-env)))))
                       
                       :else
                       (let [l' (typecheck-symbols symbols types-env)
                             f (first l')
                             args (rest l')]
                         (apply f args))))))

(defn add-types-to-env
  [symbols types-env]
  (let [env-w-types (env/env types-env)
        ;; fix: uglyhhh....
        definitions (into (:events symbols)
                          (into (into (:external (:constants symbols))
                                      (:internal (:constants symbols)))
                                (into (into (:external (:storage symbols))
                                            (:internal (:storage symbols)))
                                      (into (:external (:functions symbols))
                                            (:internal (:functions symbols))))))
                          type-defs (reduce (fn [full x]
                                              (conj full
                                                    [(:name x)
                                                     {:args (:args x)
                                                      :return (:return x)}
                                                     ])) [] definitions)]
        (doseq [[k v] type-defs] (env/eset env-w-types k v))
        env-w-types))

(defn init-local-types-env
  [types-env definition]
  (let [local-types-env (env/env types-env)]
    (doseq [[k v]
            (reduce (fn [full x] (conj
                                 full
                                 [(:name x) {:type (:type x)
                                             :mutable? (:mutable? x)}]
                                 ))
                    [] (into (:return definition) (:args definition)))
            ] (env/eset local-types-env k v))
    local-types-env))

(defn typecheck-funcs [symbols types-env]
  (let [fns (into (:external (:functions symbols))
                  (:internal (:functions symbols)))
        bodies (reduce (fn [full f] (conj full (:body f))) [] fns)]
    (loop [f fns
           b bodies]
      (if (= (count f) 0)
        (let [local-types-env (init-local-types-env types-env (first f))]
        (do (typecheck (first b) local-types-env)
            (recur (rest f) (rest b))))))))

(defn check-types
  [symbols types-env]
    (typecheck-funcs
     symbols
     (add-types-to-env symbols types-env)))
