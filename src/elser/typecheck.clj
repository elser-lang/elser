(ns elser.typecheck
  (:gen-class)
  (:require [elser.errors :as errs]
            [elser.compiler :as compiler]
            [elser.types :as types]
            [clojure.string :as string]
            [elser.env :as env]))

(declare typecheck)
(defn typecheck-symbols
  [symbols types-env permissions]
  (cond
    (symbol? symbols) (env/eget types-env symbols)
    
    (map? symbols) (let [k (keys symbols)
                         v (doall (map (fn [x] (typecheck x types-env permissions))
                                       (vals symbols)))]
                     (zipmap k v))
    
    (seq? symbols) (mapv (fn [x] (typecheck x types-env permissions))
                         symbols)
    
    (vector? symbols) (mapv (fn [x] (typecheck x types-env permissions))
                            symbols)
    
    :else {:type (types/infer-type symbols) :mutable? 'mut}))

;; FIX: this is basically evaluation function (like `compile`).
;;      Thus, it's needed to utilize existing one.
(defn typecheck
  [symbols types-env permissions]
  (cond
    ;; TODO: what to do wtih this case?
    (nil? symbols) symbols
    
    (not (seq? symbols)) (typecheck-symbols symbols types-env permissions)
    
    (empty? symbols) '()
    
    (seq? symbols) (let [f (first symbols)]
                     (cond
                       (= f 'let)
                       (let [let-env (env/env types-env)
                             local-defs (partition 2 (first (rest symbols)))]
                         (doseq [[a b] local-defs]
                           (env/eset let-env a
                                     (assoc (typecheck b types-env permissions)
                                            :mutable? 'mut)))
                         (mapv (fn [i] (typecheck (nth symbols i)
                                               let-env permissions))
                               (range 2 (count symbols))))

                       (= f 'loop)
                       (let [loop-env (env/env types-env)
                             ;; Typecheck bindings like 'let'.
                             [_ _ cnd body post-iter] symbols
                             slt (assoc (vec symbols) 0 'let)]
                         (doseq [[a b] (partition 2 (first (rest symbols)))]
                           (env/eset loop-env a
                                     (assoc (typecheck b types-env permissions)
                                            :mutable? 'mut)))
                         (do 
                           (typecheck (second cnd) loop-env permissions)
                           (typecheck post-iter loop-env permissions)
                           (typecheck body loop-env permissions)))
                       
                       ;; 'do' - evaluate all the elements of the list
                       ;; and return the final evaluated element.
                       (= f 'do)
                       (let [exprs (mapv
                                    (fn [sym]
                                      (typecheck
                                       sym
                                       types-env
                                       permissions))
                                    (rest symbols))
                             l (- (count exprs) 1)
                             last-expr (get exprs l)])

                       (= f 'invoke!)
                       ;; Check if invoked function contains
                       ;; permissions required by the function.
                       (let [p (:permissions
                                (env/eget types-env (first (rest symbols))))]
                         (do (if (not (and (>= (:w permissions) (:w p))
                                           (>= (:r permissions) (:r p))))
                               (errs/err-invalid-permissions
                                (first (rest symbols)) permissions p))
                               (:type
                              (first
                               (:return (typecheck (first (rest symbols))
                                                   types-env permissions))))))

                       (= f 'sto)
                       (let [op (second symbols)
                             sto-var (nth symbols 2)]
                         (cond
                           (= op 'write!)
                           (do (if (= (:w permissions) 0)
                                 (errs/err-invalid-permissions
                                  symbols
                                  permissions '{:w 1}))
                               (typecheck (last symbols) types-env permissions))

                           (= op 'read!)
                           (do (if (= (:r permissions) 0)
                                 (errs/err-invalid-permissions
                                  symbols
                                  permissions '{:r 1}))
                               (first
                                (:return (typecheck (last symbols) types-env permissions))))))
                       
                       :else
                       (let [l' (typecheck-symbols symbols types-env permissions)
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
                                                      :return (:return x)
                                                      :permissions (:permissions x)}
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
        bodies (reduce (fn [full f] (conj full (:body f))) [] fns)
        permissions (reduce (fn [full f] (conj full (:permissions f))) [] fns)]
    (loop [f fns
           b bodies
           p permissions]
      (if (= (count f) 0) nil ; we're done!
          (let [local-types-env (init-local-types-env types-env (first f))]
            (do (typecheck (first b) local-types-env (first p))
                (recur (rest f) (rest b) (rest p))))))))

(defn check-types
  [symbols types-env]
    (typecheck-funcs
     symbols
     (add-types-to-env symbols types-env)))
