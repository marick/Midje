(ns midje.ideas.reporting.t-report
  (:use midje.ideas.reporting.report
        [midje sweet test-util]))

;; These tests generate failures to examine. We don't want them to be
;; added to the total failure count, which should always be zero.
(without-counting-failures
 
(fact "report formatter is dynamically rebindindable"
  (binding [*report-format-config* {:single-fact-fn (fn [_] "successfully rebound")
                                    :summary-fn :irrelevant}]
    (let [raw-report (with-identity-renderer (clojure.test/old-report :irrelevant))]
      (nth raw-report 0) => #"FAIL.*some description.*foo.clj:3"
      (nth raw-report 1) => "successfully rebound")))

)  ; end without-counting-failures
