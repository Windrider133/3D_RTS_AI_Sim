(ns bench-baseline
  (:require [sim.combat :as combat]
            [sim.spec :as spec]))

(def ^:const dt 0.5)
(def ^:const separation-radius 2.0)
(def ^:const goal-x 100.0)

;; Create the ships.

(defn spawn [seed per-side types]
  (let [random (java.util.Random. seed)
        offset #(if (zero? %) (- goal-x) goal-x)
        noise #(* 30.0 (- (* 2.0 (.nextDouble random)) 1.0))]
    (vec (for [owner [0 1]
               i (range per-side)]
           {:id (+ (* owner per-side) i)
            :owner owner
            :type (types (mod i (count types)))
            :pos [(+ (offset owner) (noise)) (noise) (noise)]}))))

;; Use simple map scans for each ship.

(defn- distance [[ax ay az] [bx by bz]]
  (let [dx (- ax bx) dy (- ay by) dz (- az bz)]
    (+ (* dx dx) (* dy dy) (* dz dz))))

(defn- nearest-enemy [ship ships range]
  (->> ships
       (filter #(not= (:owner ship) (:owner %)))
       (map (fn [other] [other (distance (:pos ship) (:pos other))]))
       (filter #(<= (second %) (* range range)))
       (sort-by second)
       ffirst))

(defn- separation [ship ships]
  (reduce (fn [[x y z] friend]
            (if (and (= (:owner ship) (:owner friend))
                     (not= (:id ship) (:id friend))
                     (< (distance (:pos ship) (:pos friend))
                        (* separation-radius separation-radius)))
              (let [[sx sy sz] (:pos ship)
                    [fx fy fz] (:pos friend)]
                [(+ x (- sx fx)) (+ y (- sy fy)) (+ z (- sz fz))])
              [x y z]))
          [0.0 0.0 0.0]
          ships))

(defn- move [ship unit ships]
  (if (nearest-enemy ship ships (get-in unit [:weapon :range]))
    ship
    (let [[x y z] (:pos ship)
          [sx sy sz] (separation ship ships)
          gx (if (zero? (:owner ship)) goal-x (- goal-x))
          dx (- gx x) dy (- 0.0 y) dz (- 0.0 z)
          length (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
          scale (if (pos? length)
                  (/ (* (double (:speed unit)) dt) length)
                  0.0)]
      (assoc ship :pos [(+ x (* dx scale) (* 0.3 sx))
                        (+ y (* dy scale) (* 0.3 sy))
                        (+ z (* dz scale) (* 0.3 sz))]))))

(defn step-a [roster ships]
  (mapv #(move % (roster (:type %)) ships) ships))

(defn- run [roster ships ticks]
  (let [start (System/nanoTime)
        final (nth (iterate #(step-a roster %) ships) ticks)]
    {:ticks ticks
     :alive (count final)
     :ms (/ (- (System/nanoTime) start) 1e6)}))

;; Run the baseline.

(defn -main [& _]
  (let [roster (spec/load-roster)
        matrix (spec/load-matrix roster)
        types (:types (combat/dense-matrix roster matrix))
        ships (spawn 42 250 types)]
    (println "ships" (count ships) "types" types)
    (dotimes [_ 3]
      (let [result (run roster ships 40)]
        (println result
                 (format "=> %.3f ms/tick"
                         (/ (:ms result) (:ticks result))))))))
