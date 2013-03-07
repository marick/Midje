(ns implementation.firm-check-failures
  (:require
   [midje.config :as config])
  (:use midje.sweet
        midje.test-util))

; The namespace of the test should appear in test failure report

(unfinished g)
(defn f [] 3)

(facts "about namespaces in failure positions"
  (config/with-augmented-config {:visible-failure-namespace true}
    (capturing-failure-output
     (fact
       (f) => -3
       (provided (g) => 88))
     (fact
       @fact-output => #".clj:17\s+implementation.firm-check-failures"
       @fact-output => #".clj:16\s+implementation.firm-check-failures")))

  (config/with-augmented-config {:visible-failure-namespace false}
    (capturing-failure-output
     (fact
       (f) => -3
       (provided (g) => 88))
     (fact
       @fact-output => #".clj:26"
       @fact-output =not=> #"clj:26\s+implementation.firm-check-failures"
       @fact-output => #".clj:25"))))
