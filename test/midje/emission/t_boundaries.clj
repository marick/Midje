(ns midje.emission.t-boundaries
  (:require [midje.emission.boundaries :refer :all]
            [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.state :as state]))

(facts "about two forms of results"
  (fact "ternary output is intended for the user"
    (midje-results-to-ternary) => nil
    (provided (state/output-counters) => {:midje-failures 0, :midje-passes 0})

    (midje-results-to-ternary) => true
    (provided (state/output-counters) => {:midje-failures 0, :midje-passes 1})

    (midje-results-to-ternary) => false
    (provided (state/output-counters) => {:midje-failures 1, :midje-passes 1000}))

  (fact "counter output is mainly for `lein-midje`, but also self-documents"
    (all-test-failures-to-self-documenting-map {:fail 0 :error 0}) => {:failures 0}
    (provided
      (state/output-counters) => {:midje-failures 0})

    (all-test-failures-to-self-documenting-map {:fail 1 :error 0}) => {:failures 1}
    (provided
      (state/output-counters) => {:midje-failures 0})

    (all-test-failures-to-self-documenting-map {:fail 0 :error 1}) => {:failures 1}
    (provided
      (state/output-counters) => {:midje-failures 0})

    (all-test-failures-to-self-documenting-map {:fail 1 :error 0}) => {:failures 3}
    (provided
      (state/output-counters) => {:midje-failures 2})))



