(ns implementation.firm-check-failures
  (:use midje.sweet
        midje.test-util))

; The namespace of the test should appear in test failure report
(capturing-failure-output
  (fact (+ 1 1) => 4)
  (fact
    @fact-output => #"implementation.firm-check-failures"))