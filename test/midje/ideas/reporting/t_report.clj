(ns midje.ideas.reporting.t-report
  (:use midje.ideas.reporting.report
        midje.ideas.reporting.string-format
        [midje sweet test-util])
  (:require [clojure.test :as t]
            [clojure.test.junit :as junit]))

;; These tests generate failures to examine. We don't want them to be
;; added to the total failure count, which should always be zero.
(without-counting-failures
 
(fact "report formatter is dynamically rebindable"
  (let [message {:type :mock-expected-result-failure :description "FAILURE" :expected 1 :actual 2}]
    (binding [*report-format-config* {:single-fact-fn (fn [m] 
                                          (if (= m message)
                                            ["successfully rebound"]
                                            (report-strings m)))}]
      (with-identity-renderer (t/report message)) => ["successfully rebound"]
      )))

(fact "junit-report default is treated as failure" 
  (let [message {:type :mock-expected-result-failure :description "FAILURE" :expected 1 :actual 2}]
    (dosync (ref-set t/*report-counters* t/*initial-report-counters*))
    (binding [*report-format-config* {:single-fact-fn 
                                      (fn [m] 
                                        (if (= m message)
                                          [(:description m)]
                                          (report-strings m)))}]
      (with-identity-renderer (junit/junit-report message)) => (chatty-checker [a]
                                                                 (and (= a ["FAILURE"])
                                                                   (= {:pass 0, :test 0, :error 0, :fail 1} 
                                                                       @t/*report-counters*)))
      (provided 
        (note-failure-in-fact) => nil
        (junit/failure-el "FAILURE" 1 2) => nil
        )
      )))

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

)  ; end without-counting-failures


;(fact "junit-report :future-fact is rendered only" 
;  (let [m {:type :some-failure :description ...desc... :expected ...expected... :actual ...actual...}]
;    (junit/junit-report m) => anything
;    (provided
;      (t/inc-report-counter :fail) => nil :times 0
;      (note-failure-in-fact) => nil :times 0
;      (junit/failure-el ...desc... ...expected... ...actual...) => nil :times 0
;      (render m) => nil
;      )))

