;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-report
  (:use midje.util.report
        [midje.checkers.util :only [captured-exception]]
        [midje sweet test-util]))


(testable-privates midje.util.report)


;; This set of tests generate failures. The following code prevents
;; them from being counted as failures when the final summary is
;; printed. The disadvantage is that legitimate failures won't appear
;; in the final summary. They will, however, produce failure output,
;; so that's an acceptable compromise.

(when (nil? clojure.test/*report-counters*)
  (alter-var-root #'clojure.test/*report-counters*
                  (constantly (ref clojure.test/*initial-report-counters*))))

(background (around :facts (let [report-counters @clojure.test/*report-counters*]
                             ?form
                             (dosync (commute clojure.test/*report-counters* (constantly report-counters)))) ))


(fact "string positions have filenames and line numbers"
  (midje-position-string ["filename.clj" 33]) => "(filename.clj:33)")

;; In the case of the checker (exactly odd?), you want to see failures
;; written in terms of a function name instead of some absurdly complicated
;; #<core$even_QMARK_ clojure.core$even_QMARK_@15ee9cc3>
(future-fact "(exactly odd?) is printed attractively"
  (attractively-stringified-form even?) => "a function named 'even?'"
  (attractively-stringified-form (fn [n] 1)) => #"fn__")

(fact "rendering functional failures"
  (let [failure-map {:type :mock-expected-result-functional-failure
                     :actual 2
                     :intermediate-results [ ['(f 1) 33] ]
                     :position ["foo.clj" 3]
                     :expected '(test-checker 33)}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]

    (nth raw-report 0) => #"FAIL.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*did not agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*test-checker 33"
    (nth raw-report 4) => #"intermediate values"
    (nth raw-report 5) => #"\(f 1\) => 33")
    
  (let [failure-map {:type :mock-expected-result-functional-failure
                     :actual 2
                     :position ["foo.clj" 3]
                     :expected 'odd?}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*did not agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*odd?")

  "values in strings are formatted via pr-str"
  (let [failure-map {:type :mock-expected-result-functional-failure
               :actual nil
               :expected '(sloobom "forp")
               :intermediate-results [['(+ 1 "ate") nil]]
               :position ["foo" 23]}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 2) => #"result: nil"
    (nth raw-report 3) => #"function: \(sloobom \"forp\""
    (nth raw-report 5) => #"\(\+ 1 \"ate\"\) => nil")

  (let [failure-map {:type :mock-expected-result-functional-failure
                     :actual 2
                     :notes ["NOTE ME!" "ME TOO"]
                     :position ["foo.clj" 3]
                     :expected '(test-checker 33)}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*did not agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*test-checker 33"
    (nth raw-report 4) => #"(?i)checker.*reason"
    (nth raw-report 5) => (contains "NOTE ME!")
    (nth raw-report 6) => (contains "ME TOO")))

  
(fact "rendering inappropriate checker matches"
  (let [failure-map {:type :mock-actual-inappropriately-matches-checker
                     :actual 2
                     :position ["foo.clj" 3]
                     :expected '(test-checker 33)}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]

    (nth raw-report 0) => #"FAIL.*foo.clj:3"
    (nth raw-report 1) => #"Actual.*was NOT supposed to agree"
    (nth raw-report 2) => #"Actual.*2"
    (nth raw-report 3) => #"Checking function.*test-checker 33"))
    
(fact "excess matches"
  (let [failure-map {:type :mock-argument-match-failure
                     :actual '(nil)
                     :position ["foo.clj" 3]
                     :lhs odd?}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (prn raw-report)
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 1) => #"never said .*odd.* would be needed"
    (nth raw-report 1) =future=> #"never said odd\? would be needed"
    (nth raw-report 2) => #"\(nil\)"))

(fact "mock never called"
  (let [failure-map {:type :mock-incorrect-call-count
                     :actual-count 0
                     :position ["foo.clj" 3]
                     :expected "(f a)"}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 1) => #"claimed the following was needed"
    (nth raw-report 2) => #"\(f a\)"))

(fact "mock called an incorrect number of times"
  (let [failure-map {:type :mock-incorrect-call-count
                     :actual-count 3
                     :position ["foo.clj" 3]
                     :expected "(f a)"}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 1) => #"used three times"
    (nth raw-report 2) => #"\(f a\)"))



(fact "ordinary bad result from equality"
  (let [failure-map {:type :mock-expected-result-failure
                     :position ["foo.clj" 3]
                     :actual nil
                     :expected "s"}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 1) => #"Expected: \"s\""
    (nth raw-report 2) => #"Actual: nil"))

(fact "equality when inequality expected"
  (let [failure-map {:type :mock-expected-result-inappropriately-matched
                     :position ["foo.clj" 3]
                     :actual "s"
                     :expected "s"}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 1) => #"Expected: Anything BUT \"s\""
    (nth raw-report 2) => #"Actual: \"s\""))

(facts "about reporting exceptions"
  (let [failure-map {:type :mock-expected-result-failure
                     :position ["foo.clj" 3]
                     :actual (captured-exception (Error. "message"))
                     :expected "hi"}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    ;; Because midje stack traces are filtered out, there's not much more to check.
    (nth raw-report 2) => #"Error.*message")

  "This reporting only applies to unexpected exceptions."
  (/ 1 0) => (throws ArithmeticException))

(facts "about reporting specific user errors"
  (let [failure-map {:type :user-error
                     :notes ["message"]
                     :position ["foo.clj" 3]}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 2) => #"message"))

(facts "about reporting user errors detected because of an exception"
  (let [failure-map {:type :exceptional-user-error
                     :macro-form '(foo bar)
                     :exception-lines ["one" "two"]
                     :position ["foo.clj" 3]}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 2) => (contains "(foo bar)")
    (nth raw-report 4) => (contains "one")
    (nth raw-report 5) => (contains "two")))

(fact "binding notes are considered part of the position"
  (let [failure-map {:type :mock-expected-result-failure
                     :binding-note "a note"
                     :position ["foo.clj" 3]
                     :actual nil
                     :expected "s"}
        raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => #"FAIL at .*foo.clj:3"
    (nth raw-report 1) => #"a note"))

