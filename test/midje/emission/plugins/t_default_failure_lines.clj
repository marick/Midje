(ns midje.emission.plugins.t-default-failure-lines
  (:use [midje sweet util test-util]
        midje.emission.plugins.default-failure-lines)
  (:require [midje.emission.plugins.util :as util]))


(against-background [(util/failure-notice anything) => "notice"]

  (fact "the simplest kind of failure"
    (fact "is a mismatch" 
      (summarize {:type :mock-expected-result-failure :expected-form-to-print 1, :actual 2})
      => (just "notice" #"\s+Expected: 1", #"\s+Actual: 2")

      ;; in general...
      (summarize {:type :mock-expected-result-failure :expected-form-to-print 'expected :actual ..actual..})
      => (just "notice" #"\s+Expected: expected", #"\s+Actual: AAA")
      (provided
        (util/attractively-stringified-form ..actual..) => 'AAA))

    
    (fact "or an unexpected match" 
      (summarize {:type :mock-expected-result-inappropriately-matched :expected-form-to-print 2, :actual 2})
      => (just "notice" #"\s+Expected: Anything BUT 2", #"\s+Actual: 2")
      
      ;; in general...
      (summarize {:type :mock-expected-result-inappropriately-matched :expected-form-to-print 'expected :actual ..actual..})
      => (just "notice" #"\s+Expected: Anything BUT expected", #"\s+Actual: AAA")
      (provided
        (util/attractively-stringified-form ..actual..) => 'AAA)))


  (fact "checkers"
    (summarize {:type :mock-expected-result-functional-failure :actual 2, :expected-form-to-print 'odd?})
    => (just "notice" "Actual result did not agree with the checking function."
             #"\s+Actual result: 2" #"\s+Checking function: odd")

    (fact "can supply intermediate results"
      (summarize {:type :mock-expected-result-functional-failure :actual 2, :expected-form-to-print 'checker
                  :intermediate-results '[[(f 1) "3"] [(f 2) 3]]})
      => (just "notice" "Actual result did not agree with the checking function."
               #"\s+Actual result: 2"
               #"\s+Checking function: checker"
               #"During checking, these intermediate values were seen:"
               (contains "(f 1) => \"3\"")
               (contains "(f 2) => 3")))

    (fact "can also supply notes"
      (summarize {:type :mock-expected-result-functional-failure :actual 2, :expected-form-to-print 'checker
                  :notes ["note 1" "note 2"]})
      => (just "notice" "Actual result did not agree with the checking function."
               #"\s+Actual result: 2"
               #"\s+Checking function: checker"
               #"The checker said this about the reason:"
               #"note 1"
               #"note 2"))

    (fact "prettify the actual value and intermediate results"
      (summarize {:type :mock-expected-result-functional-failure
                  :actual {:z 2 :p 3 :r 4 :a 5 :f 6 :d 7}
                  :expected-form-to-print 'checker
                  :intermediate-results '[[(f 2) #{15 3 7 2}]]})
      => (just "notice" "Actual result did not agree with the checking function."
               #"\s+Actual result: \{:a 5, :d 7, :f 6, :p 3, :r 4, :z 2\}"
               #"\s+Checking function: checker"
               #"During checking, these intermediate values were seen:"
               (contains "(f 2) => #{2 3 7 15}")))

    (fact "can also be incorrectly matched"
      (summarize {:type :mock-actual-inappropriately-matches-checker
                  :actual {:z 2 :p 3 :r 4 :a 5 :f 6 :d 7}
                  :expected-form-to-print '(collection 3)})
      => (just "notice" "Actual result was NOT supposed to agree with the checking function."
               #"\s+Actual result: \{:a 5, :d 7, :f 6, :p 3, :r 4, :z 2\}"
               #"\s+Checking function: \(collection 3\)"))
    )
             

)  ;; Against-background
