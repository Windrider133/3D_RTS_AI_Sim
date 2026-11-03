(ns sim.spec
  "Data contracts for data/*.edn and loaders that validate on read."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]))

(s/def ::class #{:strike :corvette :frigate :capital :aux})
(s/def ::cost pos?)
(s/def ::build-time pos?)
(s/def ::hp pos?)
(s/def ::speed pos?)
(s/def ::sensor-range pos?)
(s/def ::range pos?)
(s/def ::dps pos?)
(s/def ::weapon (s/keys :req-un [::range ::dps]))
(s/def ::unit (s/keys :req-un [::class ::cost ::build-time ::hp ::speed]
                      :opt-un [::weapon ::sensor-range]))
(s/def ::roster (s/map-of keyword? ::unit))

(s/def ::multiplier (s/and number? #(<= 0.25 % 2.0)))
(s/def ::default number?)
(s/def ::overrides (s/map-of keyword? (s/map-of keyword? ::multiplier)))
(s/def ::matrix (s/keys :req-un [::default ::overrides]))

(defn- read-edn [path]
  (with-open [r (java.io.PushbackReader. (io/reader (io/file path)))]
    (edn/read r)))

(defn- check! [spec x what]
  (when-not (s/valid? spec x)
    (throw (ex-info (str "Invalid " what) {:explain (s/explain-data spec x)})))
  x)

(defn load-roster
  ([] (load-roster "data/roster.edn"))
  ([path] (check! ::roster (read-edn path) "roster")))

(defn load-matrix
  "Loads the matrix and checks that every referenced type exists in `roster`."
  ([roster] (load-matrix roster "data/matrix.edn"))
  ([roster path]
   (let [m (check! ::matrix (read-edn path) "matrix")
         known (set (keys roster))
         used (into #{} (mapcat (fn [[a row]] (cons a (keys row)))) (:overrides m))]
     (when-let [bad (seq (remove known used))]
       (throw (ex-info "Matrix references unknown types" {:unknown (vec bad)})))
     m)))
