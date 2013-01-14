(ns midje.ideas.reporting.t-string-format-map-translation
  (:use midje.ideas.reporting.report
        [midje.ideas.reporting.string-format :only [midje-position-string]]
        [midje.error-handling.exceptions :only [captured-throwable]]
        [midje sweet test-util]
        midje.util))

(expose-testables midje.ideas.reporting.string-format)


;; These tests generate failures to examine. We don't want them to be
;; added to the total failure count, which should always be zero.
(without-changing-cumulative-totals

(fact "rendering functional failures"
  (let [failure-map {:type :mock-expected-result-functional-failure
                     :description ["some description"]
                     :actual 2
                     :intermediate-results [ ['(f 1) 33] ]
                     :position ["foo.clj" 3]
                     :expected '(test-checker 33)}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]

    (nth raw-report 0) => #"FAIL.*some description.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*did not agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*test-checker 33"
    (nth raw-report 4) => #"intermediate values"
    (nth raw-report 5) => #"\(f 1\) => 33")
    
  (let [failure-map {:type :mock-expected-result-functional-failure
                     :description ["some description"]
                     :actual 2
                     :position ["foo.clj" 3]
                     :expected 'odd?}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*did not agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*odd?")

  "values in strings are formatted via pr-str"
  (let [failure-map {:type :mock-expected-result-functional-failure
                     :description ["some description"]
                     :actual nil
                     :expected '(sloobom "forp")
                     :intermediate-results [['(+ 1 "ate") nil]]
                     :position ["foo" 23]}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 2) => #"result: nil"
    (nth raw-report 3) => #"function: \(sloobom \"forp\""
    (nth raw-report 5) => #"\(\+ 1 \"ate\"\) => nil")

  (let [failure-map {:type :mock-expected-result-functional-failure
                     :description ["some description"]
                     :actual 2
                     :notes ["NOTE ME!" "ME TOO"]
                     :position ["foo.clj" 3]
                     :expected '(test-checker 33)}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*did not agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*test-checker 33"
    (nth raw-report 4) => #"(?i)checker.*reason"
    (nth raw-report 5) => (contains "NOTE ME!")
    (nth raw-report 6) => (contains "ME TOO")))

  
(fact "rendering inappropriate checker matches"
  (let [failure-map {:type :mock-actual-inappropriately-matches-checker
                     :description ["some description"]
                     :actual 2
                     :position ["foo.clj" 3]
                     :expected '(test-checker 33)}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]

    (nth raw-report 0) => #"FAIL.*some description.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*was NOT supposed to agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*test-checker 33"
    ))
    
(fact "excess matches"
  (let [failure-map {:type :mock-argument-match-failure
                     :description ["some description"]
                     :actual '(nil)
                     :position ["foo.clj" 3]
                     :var odd?}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 1) => #"never said .*odd.* would be needed"
    (nth raw-report 1) =future=> #"never said odd\? would be needed"
    (nth raw-report 2) => #"\(nil\)"))

(fact "mock never called"
  (let [failure-map {:type :mock-incorrect-call-count
                     :failures [{ :description ["some description"]
                                  :actual-count 0
                                  :expected-count nil
                                  :position ["foo.clj" 3]
                                  :expected "(f a)"}] }
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 1) => #"These calls were not made the right number of times"
    (nth raw-report 2) => #"\(f a\)"
    (nth raw-report 2) => #"expected at least once"))

(fact "mock called an incorrect number of times"
  (let [failure-map {:type :mock-incorrect-call-count
                     :failures [{ :description ["some description"]
                                  :actual-count 3
                                  :expected-count 1
                                  :position ["foo.clj" 3]
                                  :expected "(f a)" }] }
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 1) => #"These calls were not made the right number of times"
    (nth raw-report 2) => #"\(f a\)"
    (nth raw-report 2) => #"expected :times 1"))




(fact "equality when inequality expected"
  (let [failure-map {:type :mock-expected-result-inappropriately-matched
                     :description ["some description"]
                     :position ["foo.clj" 3]
                     :actual "s"
                     :expected "s"}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 1) => #"Expected: Anything BUT \"s\""
    (nth raw-report 2) => #"Actual: \"s\""))

(facts "about reporting specific user errors"
  (let [failure-map {:type :validation-error
                     :description ["some description"]
                     :notes ["message"]
                     :position ["foo.clj" 3]}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 2) => #"message"))

(facts "about reporting user errors detected because of an exception"
  (let [failure-map {:type :exceptional-user-error
                     :description ["some description"]
                     :macro-form '(foo bar)
                     :stacktrace ["one" "two"]
                     :position ["foo.clj" 3]}
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 2) => (contains "(foo bar)")
    (nth raw-report 4) => (contains "one")
    (nth raw-report 5) => (contains "two")))


)  ; end without-changing-cumulative-totals
