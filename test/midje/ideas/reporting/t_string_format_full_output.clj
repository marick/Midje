(ns midje.ideas.reporting.t-string-format-full-output
  (:use midje.ideas.reporting.report
        [midje sweet test-util]))

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


(let [output (with-out-str
               (fact (+ 1 1) => 3))]
  (fact
    output => #"FAIL"
    output => #"Expected:\s+3"
    output => #"Actual:\s+2"))


(let [output (with-out-str
               (fact (+ 1 1) =not=> 2))]
  (fact
    output => #"FAIL"
    output => #"Expected: Anything BUT 2"
    output => #"Actual:\s+2"))

(let [output (with-out-str
               (fact (+ 1 1) => odd?))]
  (fact
    output => #"FAIL"
    output => #"checking function"
    output => #"Actual result:\s+2"
    output => #"Checking function:\s+odd\?"))

(let [output (with-out-str
               (fact (+ 1 1) =not=> even?))]
  (fact
    output => #"FAIL"
    output => #"NOT supposed to agree.*checking function"
    output => #"Actual result:\s+2"
    output => #"Checking function:\s+even\?"))

(let [output (with-out-str
               (fact (cons 1 nil) => (contains 2)))]
  (fact
    output => #"FAIL"
    output => #"checking function"
    output => #"Actual result:\s+\(1\)"
    output => #"Checking function:\s+\(contains 2\)"
    output => #"Best match found:\s+\[\]"))

(let [output (with-out-str
               (fact (cons 1 nil) =not=> (just 1)))]
  (fact
    output => #"FAIL"
    output => #"NOT supposed to agree.*checking function"
    output => #"Actual result:\s\(1\)"
    output => #"Checking function:\s+\(just 1\)"))

(let [output (with-out-str
               (fact 5 => (chatty-checker [a] (and (= a 5) (= a 6)))))]
  (fact
    output => #"FAIL"
    output => #"checking function"
    output => #"Actual result:\s+5"
    output => #"Checking function:\s+\(chatty-checker \[a\] \(and \(= a 5\) \(= a 6\)\)\)"
    output => #"\(= a 5\) => true"
    output => #"\(= a 6\) => false"))


(let [output (with-out-str
               (fact 5 => (every-checker even? (throws "message"))))]
  (fact
    output => #"FAIL"
    output => #"checking function"
    output => #"Actual result:\s+5"
    output => #"Checking function:\s+\(every-checker even\? \(throws \"message\"\)\)"
    output => #"even\? => false"))
