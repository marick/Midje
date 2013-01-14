(ns midje.emission.plugins.t-default-end-to-end
  (:use midje.ideas.reporting.report
        [midje sweet test-util])
  (:require [midje.ideas.reporting.string-format :as kludge]))

(capturing-failure-output
 (fact (+ 1 1) => 3)
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"Expected:\s+3"
   @fact-output => #"Actual:\s+2"))

(capturing-failure-output
 (fact (+ 1 1) =not=> 2)
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"Expected: Anything BUT 2"
   @fact-output => #"Actual:\s+2"))

(capturing-failure-output
 (fact (+ 1 1) => odd?)
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"checking function"
   @fact-output => #"Actual result:\s+2"
   @fact-output => #"Checking function:\s+odd\?"))

(capturing-failure-output
 (fact (+ 1 1) =not=> even?)
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"NOT supposed to agree.*checking function"
   @fact-output => #"Actual result:\s+2"
   @fact-output => #"Checking function:\s+even\?"))

(println "finish these")

;; (let [output (with-out-str
;;                (fact (cons 1 nil) => (contains 2)))]
;;   (fact
;;     @fact-output => #"FAIL"
;;     @fact-output => #"checking function"
;;     @fact-output => #"Actual result:\s+\(1\)"
;;     @fact-output => #"Checking function:\s+\(contains 2\)"
;;     @fact-output => #"Best match found:\s+\[\]"))

;; (let [output (with-out-str
;;                (fact (cons 1 nil) =not=> (just 1)))]
;;   (fact
;;     @fact-output => #"FAIL"
;;     @fact-output => #"NOT supposed to agree.*checking function"
;;     @fact-output => #"Actual result:\s\(1\)"
;;     @fact-output => #"Checking function:\s+\(just 1\)"))

;; (let [output (with-out-str
;;                (fact 5 => (chatty-checker [a] (and (= a 5) (= a 6)))))]
;;   (fact
;;     @fact-output => #"FAIL"
;;     @fact-output => #"checking function"
;;     @fact-output => #"Actual result:\s+5"
;;     @fact-output => #"Checking function:\s+\(chatty-checker \[a\] \(and \(= a 5\) \(= a 6\)\)\)"
;;     @fact-output => #"\(= a 5\) => true"
;;     @fact-output => #"\(= a 6\) => false"))


;; (let [output (with-out-str
;;                (fact 5 => (every-checker even? (throws "message"))))]
;;   (fact
;;     @fact-output => #"FAIL"
;;     @fact-output => #"checking function"
;;     @fact-output => #"Actual result:\s+5"
;;     @fact-output => #"Checking function:\s+\(every-checker even\? \(throws \"message\"\)\)"
;;     @fact-output => #"even\? => false"))

