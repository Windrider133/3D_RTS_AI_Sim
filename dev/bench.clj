(ns bench
  "Day-1 benchmark spike (throwaway). 500 ships, two swarms, minimal step:
  move toward the enemy start point, hit the nearest enemy in range, separate from friends.
  Variant A: persistent maps + O(n^2) scans. Variant B: primitive arrays + spatial hash grid."
  (:require [sim.combat :as combat]
            [sim.spec :as spec]))

(set! *warn-on-reflection* true)

(def ^:const dt 0.5)
(def ^:const sep-radius 2.0)
(def ^:const goal-x 100.0)

;; ---------------------------------------------------------------- spawn

(defn spawn
  "Vector of ship maps, n per side, types round-robin over combat types, in a ball of radius 30."
  [seed per-side types]
  (let [rnd (java.util.Random. seed)
        nt (count types)]
    (vec (for [owner [0 1]
               i (range per-side)
               :let [cx (if (zero? owner) (- goal-x) goal-x)
                     ball #(* 30.0 (- (* 2.0 (.nextDouble rnd)) 1.0))]]
           {:id (+ (* owner per-side) i) :owner owner :type (types (mod i nt))
            :pos [(+ cx (ball)) (ball) (ball)] :hp nil}))))

;; ---------------------------------------------------------------- variant A

(defn- d2 ^double [[x1 y1 z1] [x2 y2 z2]]
  (let [dx (- (double x1) (double x2)) dy (- (double y1) (double y2)) dz (- (double z1) (double z2))]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn step-a [roster dm ships]
  (let [moved
        (mapv
         (fn [s]
           (let [u (roster (:type s))
                 r (double (get-in u [:weapon :range]))
                 r2 (* r r)
                 [tgt _] (reduce (fn [[b bd] e]
                                   (if (= (:owner e) (:owner s))
                                     [b bd]
                                     (let [d (d2 (:pos s) (:pos e))]
                                       (if (and (< d bd) (<= d r2)) [e d] [b bd]))))
                                 [nil Double/MAX_VALUE] ships)
                 hit (when tgt [(:id tgt) (* (double (get-in u [:weapon :dps])) dt
                                             (combat/multiplier dm (:type s) (:type tgt)))])
                 [sx sy sz] (reduce (fn [[ax ay az] f]
                                      (if (and (= (:owner f) (:owner s)) (not= (:id f) (:id s))
                                               (< (d2 (:pos s) (:pos f)) (* sep-radius sep-radius)))
                                        (let [[px py pz] (:pos s) [qx qy qz] (:pos f)]
                                          [(+ ax (- px qx)) (+ ay (- py qy)) (+ az (- pz qz))])
                                        [ax ay az]))
                                    [0.0 0.0 0.0] ships)
                 [px py pz] (:pos s)
                 gx (if (zero? (:owner s)) goal-x (- goal-x))
                 step (* (double (:speed u)) dt)
                 pos (if tgt
                       [px py pz]
                       (let [dx (- gx px) dy (- 0.0 py) dz (- 0.0 pz)
                             len (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                             k (if (pos? len) (/ step len) 0.0)]
                         [(+ px (* dx k) (* 0.3 sx)) (+ py (* dy k) (* 0.3 sy)) (+ pz (* dz k) (* 0.3 sz))]))]
             [(assoc s :pos pos) hit]))
         ships)
        dmg (reduce (fn [m [_ [id v]]] (if id (update m id (fnil + 0.0) v) m)) {}
                    (map (fn [[_ h]] [nil h]) moved))]
    (into [] (keep (fn [[s _]]
                     (let [hp (- (double (:hp s)) (double (get dmg (:id s) 0.0)))]
                       (when (pos? hp) (assoc s :hp hp)))))
          moved)))

(defn init-a [roster ships]
  (mapv #(assoc % :hp (double (get-in roster [(:type %) :hp]))) ships))

;; ---------------------------------------------------------------- variant B

(def ^:const grid-n 24)
(def ^:const cell 30.0)
(def ^:const origin -360.0)

(defn init-b [roster dm ships]
  (let [n (count ships)
        idx (:index dm)
        st {:n n
            :x (double-array (map #(double (get-in % [:pos 0])) ships))
            :y (double-array (map #(double (get-in % [:pos 1])) ships))
            :z (double-array (map #(double (get-in % [:pos 2])) ships))
            :hp (double-array (map #(double (get-in roster [(:type %) :hp])) ships))
            :owner (int-array (map :owner ships))
            :type (int-array (map #(idx (:type %)) ships))
            :range (double-array (map #(get-in roster [% :weapon :range]) (:types dm)))
            :dps (double-array (map #(get-in roster [% :weapon :dps]) (:types dm)))
            :speed (double-array (map #(get-in roster [% :speed]) (:types dm)))
            :dmg (double-array n)
            :cell-of (int-array n)
            :cell-start (int-array (inc (* grid-n grid-n grid-n)))
            :items (int-array n)
            :acc (double-array 5)}]
    st))

(defn- cell-index ^long [^double v]
  (let [c (long (Math/floor (/ (- v origin) cell)))]
    (max 0 (min (dec grid-n) c))))

(defn step-b!
  "Mutates `st` in place. Returns the number of living ships."
  ^long [st ^doubles mat ^long nt]
  (let [n (long (:n st))
        ^doubles x (:x st) ^doubles y (:y st) ^doubles z (:z st)
        ^doubles hp (:hp st) ^ints owner (:owner st) ^ints typ (:type st)
        ^doubles rng (:range st) ^doubles dps (:dps st) ^doubles spd (:speed st)
        ^doubles dmg (:dmg st) ^ints cell-of (:cell-of st)
        ^ints cstart (:cell-start st) ^ints items (:items st)
        ^doubles acc (:acc st)
        ncell (* grid-n grid-n grid-n)]
    ;; --- build grid by counting sort
    (java.util.Arrays/fill cstart 0)
    (dotimes [i n]
      (when (pos? (aget hp i))
        (let [c (+ (* (+ (* (cell-index (aget x i)) grid-n) (cell-index (aget y i))) grid-n)
                   (cell-index (aget z i)))]
          (aset cell-of i (int c))
          (aset cstart (inc c) (inc (aget cstart (inc c)))))))
    (dotimes [c ncell]
      (aset cstart (inc c) (+ (aget cstart (inc c)) (aget cstart c))))
    (let [fill (java.util.Arrays/copyOf cstart (int ncell))]
      (dotimes [i n]
        (when (pos? (aget hp i))
          (let [c (aget cell-of i) p (aget fill c)]
            (aset items p (int i))
            (aset fill c (inc p))))))
    ;; --- targeting, damage accumulation, separation
    (java.util.Arrays/fill dmg 0.0)
    (let [alive (long-array 1)]
      (dotimes [i n]
        (when (pos? (aget hp i))
          (aset alive 0 (inc (aget alive 0)))
          (let [xi (aget x i) yi (aget y i) zi (aget z i)
                oi (aget owner i) ti (aget typ i)
                r (aget rng ti) r2 (* r r)
                ci (long (cell-index xi)) cj (long (cell-index yi)) ck (long (cell-index zi))]
            (aset acc 0 Double/MAX_VALUE) (aset acc 1 -1.0)
            (aset acc 2 0.0) (aset acc 3 0.0) (aset acc 4 0.0)
            (loop [a (max 0 (dec ci))]
              (when (<= a (min (dec grid-n) (inc ci)))
                (loop [b (max 0 (dec cj))]
                  (when (<= b (min (dec grid-n) (inc cj)))
                    (loop [c (max 0 (dec ck))]
                      (when (<= c (min (dec grid-n) (inc ck)))
                        (let [cid (+ (* (+ (* a grid-n) b) grid-n) c)]
                          (loop [p (aget cstart cid)]
                            (when (< p (aget cstart (inc cid)))
                              (let [j (aget items p)]
                                (when (not= j i)
                                  (let [dx (- (aget x j) xi) dy (- (aget y j) yi) dz (- (aget z j) zi)
                                        d (+ (* dx dx) (* dy dy) (* dz dz))]
                                    (if (== (aget owner j) oi)
                                      (when (< d (* sep-radius sep-radius))
                                        (aset acc 2 (- (aget acc 2) dx))
                                        (aset acc 3 (- (aget acc 3) dy))
                                        (aset acc 4 (- (aget acc 4) dz)))
                                      (when (and (<= d r2) (< d (aget acc 0)))
                                        (aset acc 0 d) (aset acc 1 (double j))))))
                                (recur (inc p))))))
                        (recur (inc c))))
                  (recur (inc b))))
              (recur (inc a))))
            (let [j (long (aget acc 1))]
              (if (>= j 0)
                (let [m (aget mat (+ (* ti nt) (aget typ j)))]
                  (aset dmg j (+ (aget dmg j) (* (aget dps ti) dt m))))
                (let [gx (if (zero? oi) goal-x (- goal-x))
                      dx (- gx xi) dy (- 0.0 yi) dz (- 0.0 zi)
                      len (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                      k (if (pos? len) (/ (* (aget spd ti) dt) len) 0.0)]
                  (aset x i (+ xi (* dx k) (* 0.3 (aget acc 2))))
                  (aset y i (+ yi (* dy k) (* 0.3 (aget acc 3))))
                  (aset z i (+ zi (* dz k) (* 0.3 (aget acc 4))))))))))
      (dotimes [i n]
        (aset hp i (- (aget hp i) (aget dmg i))))
      (aget alive 0))))

;; ---------------------------------------------------------------- runners

(defn- now-ms ^double [] (/ (double (System/nanoTime)) 1e6))

(defn run-a [roster dm ships ticks]
  (let [t0 (now-ms)
        end (loop [s (init-a roster ships) k 0]
              (if (or (== k ticks) (empty? s)) {:ticks k :alive (count s)} (recur (step-a roster dm s) (inc k))))]
    (assoc end :ms (- (now-ms) t0))))

(defn run-b
  "Runs until one side is gone or max-ticks. Returns ticks, alive per side, wall ms."
  [roster dm ships max-ticks]
  (let [st (init-b roster dm ships)
        ^doubles mat (:mat dm) nt (long (:n dm))
        t0 (now-ms)
        ticks (loop [k 0]
                (if (== k max-ticks)
                  k
                  (do (step-b! st mat nt)
                      (let [^doubles hp (:hp st) ^ints ow (:owner st)
                            side (long-array 2)]
                        (dotimes [i (:n st)]
                          (when (pos? (aget hp i)) (aset side (aget ow i) (inc (aget side (aget ow i))))))
                        (if (or (zero? (aget side 0)) (zero? (aget side 1))) (inc k) (recur (inc k)))))))
        ^doubles hp (:hp st) ^ints ow (:owner st)
        alive (reduce (fn [v i] (if (pos? (aget hp i)) (update v (aget ow i) inc) v)) [0 0] (range (:n st)))]
    {:ticks ticks :alive alive :ms (- (now-ms) t0)}))

(defn -main [& _]
  (let [roster (spec/load-roster)
        matrix (spec/load-matrix roster)
        dm (combat/dense-matrix roster matrix)
        types (:types dm)
        ships (spawn 42 250 types)
        max-ticks 7200]
    (println "ships" (count ships) "types" types)
    (println "-- warmup B")
    (dotimes [_ 3] (run-b roster dm ships 400))
    (println "-- B (grid, arrays), full match, 3 runs")
    (dotimes [_ 3]
      (let [r (run-b roster dm ships max-ticks)]
        (println r (format "=> %.3f ms/tick" (/ (:ms r) (:ticks r))))))
    (println "-- B on the same window as A (40 ticks) and 600 ticks")
    (dotimes [_ 3]
      (doseq [k [40 600]]
        (let [r (run-b roster dm ships k)]
          (println k (:alive r) (format "=> %.3f ms/tick" (/ (:ms r) (:ticks r)))))))
    (println "-- A (maps, O(n^2)), 40 ticks")
    (run-a roster dm ships 5)
    (let [r (run-a roster dm ships 40)]
      (println r (format "=> %.1f ms/tick, extrapolated 7200 ticks = %.0f s"
                         (/ (:ms r) (:ticks r)) (/ (* 7200 (/ (:ms r) (:ticks r))) 1000.0))))
    (shutdown-agents)))
