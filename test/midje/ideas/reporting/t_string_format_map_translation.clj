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
