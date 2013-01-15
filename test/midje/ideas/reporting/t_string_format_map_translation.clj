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
                                  :expected-result-form "(f a)"}] }
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
                                  :expected-result-form "(f a)" }] }
        raw-report (with-identity-renderer (clojure.test/report failure-map))]
    (nth raw-report 0) => #"FAIL.*some description.* at .*foo.clj:3"
    (nth raw-report 1) => #"These calls were not made the right number of times"
    (nth raw-report 2) => #"\(f a\)"
    (nth raw-report 2) => #"expected :times 1"))




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
