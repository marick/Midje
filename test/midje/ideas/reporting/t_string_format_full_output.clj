(ns midje.ideas.reporting.t-string-format-full-output
  (:use midje.ideas.reporting.report
        [midje sweet test-util])
  (:require [midje.ideas.reporting.string-format :as kludge]))

;; These tests generate failures to examine. We don't want them to be
;; added to the total failure count, which should always be zero.
(without-changing-cumulative-totals

 (println "delete string-format-full-output tests")


;; (let [output (with-out-str
;;                (fact (+ 1 1) =not=> 2))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"Expected: Anything BUT 2"
;;     output => #"Actual:\s+2"))

;; (let [output (with-out-str
;;                (fact (+ 1 1) => odd?))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"checking function"
;;     output => #"Actual result:\s+2"
;;     output => #"Checking function:\s+odd\?"))

;; (let [output (with-out-str
;;                (fact (+ 1 1) =not=> even?))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"NOT supposed to agree.*checking function"
;;     output => #"Actual result:\s+2"
;;     output => #"Checking function:\s+even\?"))

;; (let [output (with-out-str
;;                (fact (cons 1 nil) => (contains 2)))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"checking function"
;;     output => #"Actual result:\s+\(1\)"
;;     output => #"Checking function:\s+\(contains 2\)"
;;     output => #"Best match found:\s+\[\]"))

;; (let [output (with-out-str
;;                (fact (cons 1 nil) =not=> (just 1)))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"NOT supposed to agree.*checking function"
;;     output => #"Actual result:\s\(1\)"
;;     output => #"Checking function:\s+\(just 1\)"))

;; (let [output (with-out-str
;;                (fact 5 => (chatty-checker [a] (and (= a 5) (= a 6)))))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"checking function"
;;     output => #"Actual result:\s+5"
;;     output => #"Checking function:\s+\(chatty-checker \[a\] \(and \(= a 5\) \(= a 6\)\)\)"
;;     output => #"\(= a 5\) => true"
;;     output => #"\(= a 6\) => false"))


;; (let [output (with-out-str
;;                (fact 5 => (every-checker even? (throws "message"))))]
;;   (fact
;;     output => #"FAIL"
;;     output => #"checking function"
;;     output => #"Actual result:\s+5"
;;     output => #"Checking function:\s+\(every-checker even\? \(throws \"message\"\)\)"
;;     output => #"even\? => false"))

;; (facts "about recording error counts"
;;   (kludge/previous-failure-count) => 0

;;   (with-out-str (kludge/report-strings-summary {:fail 1, :pass 12}))
;;   (kludge/previous-failure-count) => 1

;;   (with-out-str (kludge/report-strings-summary {:fail 0, :pass 0}))
;;   (kludge/previous-failure-count) => 0

;;   (with-out-str (kludge/report-strings-summary {:fail 1, :pass 12} {:test-count 0 :fail-count 0}))
;;   (kludge/previous-failure-count) => 1

;;   (with-out-str (kludge/report-strings-summary {:fail 12, :pass 12} {:test-count 0}))
;;   (kludge/previous-failure-count) => 12

;;   (with-out-str (kludge/report-strings-summary {:fail 1, :pass 12} {:test-count 3 :fail-count 3}))
;;   (kludge/previous-failure-count) => 4)


)  ; end without-changing-cumulative-totals
