(ns sim.spec-test
  (:require [clojure.java.io :as io]
            [midje.sweet :refer [fact throws]]
            [sim.spec :as spec]))

(defn- with-temp-edn [value f]
  (let [file (java.io.File/createTempFile "sim-spec-test-" ".edn")]
    (try
      (spit file (pr-str value))
      (f (.getPath file))
      (finally
        (io/delete-file file true)))))

(def valid-unit
  {:class :strike
   :cost 10
   :build-time 1
   :hp 20
   :speed 2.0})

(fact "load-roster loads the repository roster"
  (let [roster (spec/load-roster)]
    (set (keys roster)) => #{:interceptor :bomber :flak-corvette
                             :gunship-corvette :aa-frigate :anticap-frigate
                             :capital :collector :probe}
    (get-in roster [:capital :hp]) => 6000
    (get-in roster [:probe :sensor-range]) => 40.0))

(fact "load-roster accepts a valid minimal roster"
  (with-temp-edn {:fighter valid-unit}
    (fn [path]
      (spec/load-roster path) => {:fighter valid-unit})))

(fact "load-roster rejects a unit missing a required field"
  (with-temp-edn {:fighter (dissoc valid-unit :hp)}
    (fn [path]
      (spec/load-roster path) => (throws clojure.lang.ExceptionInfo))))

(fact "load-matrix accepts the inclusive multiplier boundaries"
  (let [roster {:fighter valid-unit}
        matrix {:default 1.0
                :overrides {:fighter {:fighter 0.25}}}
        upper-bound {:default 1.0
                     :overrides {:fighter {:fighter 2.0}}}]
    (with-temp-edn matrix
      (fn [path]
        (spec/load-matrix roster path) => matrix))
    (with-temp-edn upper-bound
      (fn [path]
        (spec/load-matrix roster path) => upper-bound))))

(fact "load-matrix rejects a multiplier outside the inclusive bounds"
  (let [roster {:fighter valid-unit}]
    (with-temp-edn {:default 1.0
                    :overrides {:fighter {:fighter 0.249}}}
      (fn [path]
        (spec/load-matrix roster path) => (throws clojure.lang.ExceptionInfo)))
    (with-temp-edn {:default 1.0
                    :overrides {:fighter {:fighter 2.001}}}
      (fn [path]
        (spec/load-matrix roster path) => (throws clojure.lang.ExceptionInfo)))))

(fact "load-matrix rejects overrides for unknown roster types"
  (let [roster {:fighter valid-unit}]
    (with-temp-edn {:default 1.0
                    :overrides {:unknown {:fighter 1.0}}}
      (fn [path]
        (spec/load-matrix roster path) => (throws clojure.lang.ExceptionInfo)))
    (with-temp-edn {:default 1.0
                    :overrides {:fighter {:unknown 1.0}}}
      (fn [path]
        (spec/load-matrix roster path) => (throws clojure.lang.ExceptionInfo)))))
