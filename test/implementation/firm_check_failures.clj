(ns implementation.firm-check-failures
  (:require
   [midje.config :as config])
  (:use midje.sweet
        midje.test-util))

; The namespace of the test should appear in test failure report

(config/with-augmented-config {:visible-failure-namespace true}
  (capturing-failure-output
   (fact (+ 1 1) => 4)
   (fact
     @fact-output => #"implementation.firm-check-failures")))