(ns midje.ideas.reporting.t-report
  (:use midje.ideas.reporting.report
        midje.ideas.reporting.string-format
        [midje sweet test-util])
  (:require [clojure.test :as t]
            [clojure.test.junit :as junit]))


;; These tests generate failures to examine. We don't want them to be
;; added to the total failure count, which should always be zero.
(without-changing-cumulative-totals
 
(fact "junit-report future-fact is rendered" 
  (let [message {:type :future-fact :description "FUTURE FACT"}]
    (binding [*report-format-config* {:single-fact-fn 
                                      (fn [m] 
                                        (if (= m message)
                                          [(:description m)]
                                          (report-strings m)))}]
      (with-identity-renderer (junit/junit-report message)) => ["FUTURE FACT"]
      )))

(fact "junit-report summary is ignored" 
  (let [message {:type :summary :description "FUTURE FACT"}]
    (junit/junit-report message)) => nil)

)  ; end without-changing-cumulative-totals
