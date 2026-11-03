(ns sim.combat
  "Combat data helpers shared by the simulator and the benchmark spike.")

(defn combat-types
  "Deterministically ordered vector of types that carry a weapon."
  [roster]
  (->> roster (filter (fn [[_ u]] (:weapon u))) (map key) sort vec))

(defn dense-matrix
  "Returns {:types [...] :index {type i} :n n :mat double-array(n*n)}, row = attacker."
  [roster matrix]
  (let [types (combat-types roster)
        index (zipmap types (range))
        n (count types)
        mat (double-array (* n n) (double (:default matrix)))]
    (doseq [[a row] (:overrides matrix)
            [t v] row
            :when (and (index a) (index t))]
      (aset mat (+ (* (index a) n) (index t)) (double v)))
    {:types types :index index :n n :mat mat}))

(defn multiplier ^double [{:keys [index n ^doubles mat]} a t]
  (aget mat (+ (* (long (index a)) (long n)) (long (index t)))))
