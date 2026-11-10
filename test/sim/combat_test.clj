(ns sim.combat-test
  (:require [midje.sweet :refer [fact]]
            [sim.combat :as combat]))

(fact "combat-types returns an empty vector for an empty roster"
  (combat/combat-types {}) => [])

(fact "combat-types returns weapon-bearing types in deterministic sorted order"
  (combat/combat-types {:fighter {:weapon {}}
                        :capital {:weapon {}}
                        :collector {}})
  => [:capital :fighter])

(fact "combat-types excludes units whose weapon is nil"
  (combat/combat-types {:fighter {:weapon {}}
                        :collector {:weapon nil}
                        :probe {}})
  => [:fighter])

(fact "dense-matrix creates a one-cell matrix for one combat type"
  (let [dense (combat/dense-matrix
               {:fighter {:weapon {}}}
               {:default 0.75
                :overrides {}})]
    (select-keys dense [:types :index :n]) => {:types [:fighter]
                                               :index {:fighter 0}
                                               :n 1}
    (combat/multiplier dense :fighter :fighter) => 0.75))

(fact "dense-matrix creates an empty matrix for a roster without weapons"
  (let [dense (combat/dense-matrix
               {:collector {}
                :probe {}}
               {:default 1.0
                :overrides {}})]
    (select-keys dense [:types :index :n]) => {:types []
                                               :index {}
                                               :n 0}))

(fact "multiplier uses the default for every missing matrix pair"
  (let [dense (combat/dense-matrix
               {:fighter {:weapon {}}
                :capital {:weapon {}}}
               {:default 0.75
                :overrides {}})]
    (combat/multiplier dense :capital :fighter) => 0.75
    (combat/multiplier dense :fighter :capital) => 0.75
    (combat/multiplier dense :capital :capital) => 0.75
    (combat/multiplier dense :fighter :fighter) => 0.75))

(fact "multiplier returns an override for the requested attacker and target"
  (let [dense (combat/dense-matrix
               {:fighter {:weapon {}}
                :capital {:weapon {}}}
               {:default 1.0
                :overrides {:fighter {:capital 1.5}}})]
    (combat/multiplier dense :fighter :capital) => 1.5
    (combat/multiplier dense :capital :fighter) => 1.0))

(fact "dense-matrix stores multiple overrides independently"
  (let [dense (combat/dense-matrix
               {:fighter {:weapon {}}
                :capital {:weapon {}}
                :corvette {:weapon {}}}
               {:default 1.0
                :overrides {:fighter {:capital 1.5
                                      :corvette 0.5}
                            :capital {:fighter 1.25}}})]
    (combat/multiplier dense :fighter :capital) => 1.5
    (combat/multiplier dense :fighter :corvette) => 0.5
    (combat/multiplier dense :capital :fighter) => 1.25
    (combat/multiplier dense :capital :corvette) => 1.0
    (combat/multiplier dense :corvette :fighter) => 1.0))

(fact "dense-matrix preserves a zero-valued override"
  (let [dense (combat/dense-matrix
               {:fighter {:weapon {}}}
               {:default 1.0
                :overrides {:fighter {:fighter 0.0}}})]
    (combat/multiplier dense :fighter :fighter) => 0.0))

(fact "dense-matrix ignores overrides that reference unknown types"
  (let [dense (combat/dense-matrix
               {:fighter {:weapon {}}}
               {:default 1.0
                :overrides {:unknown {:fighter 2.0}
                            :fighter {:unknown 2.0}}})]
    (select-keys dense [:types :index :n]) => {:types [:fighter]
                                               :index {:fighter 0}
                                               :n 1}
    (combat/multiplier dense :fighter :fighter) => 1.0))
