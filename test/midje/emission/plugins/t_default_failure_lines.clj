(ns midje.emission.plugins.t-default-failure-lines
  (:use [midje sweet util test-util]
        midje.emission.plugins.default-failure-lines)
  (:require [midje.emission.plugins.util :as util]))


(against-background [(util/failure-notice anything) => "notice"]

  (fact "the simplest kind of failure" 
    (summarize {:type :mock-expected-result-failure :expected-form-to-print 'odd?, :actual 2})
    => (just "notice" #"\s+Expected: odd\?", #"\s+Actual: 2")

    ;; in general...
    (summarize {:type :mock-expected-result-failure :expected-form-to-print 'expected :actual ..actual..})
    => (just "notice" #"\s+Expected: expected", #"\s+Actual: AAA")
    (provided
      (util/attractively-stringified-form ..actual..) => 'AAA))
)              
