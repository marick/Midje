(ns midje.util.t-report-test
  (:use [midje.util.report] :reload-all)
  (:use [midje.sweet])
  (:use [clojure.test])
  (:use [midje.test-util]))


(testable-privates midje.util.report midje-position-string functional-failure-lines
		   without-nasty-looking-functions)

(fact "string positions have filenames and line numbers"
  (midje-position-string ["filename.clj" 33]) => "(filename.clj:33)")

(defn re [expected]
  (fn [actual] (re-find expected actual)))

;; In the case of the checker (exactly odd?), you want to see failures
;; written in terms of a function name instead of some absurdly complicated
;; #<core$even_QMARK_ clojure.core$even_QMARK_@15ee9cc3>
(fact "(exactly odd?) is printed attractively"
  (without-nasty-looking-functions even?) => "a function named 'even?'"
  (without-nasty-looking-functions (fn [n] 1)) => (re #"fn__"))


(fact "rendering functional failures"
  (let [failure-map {:type :mock-expected-result-functional-failure
		     :actual 2
		     :intermediate-results [ ['(f 1) 33] ]
		     :position ["foo.clj" 3]
		     :expected '(test-checker 33)}
	raw-report (with-identity-renderer (clojure.test/old-report failure-map))]

    (nth raw-report 0) => (re #"FAIL.*foo.clj:3")
    (nth raw-report 1) => (re #"Actual.*did not agree")
    (nth raw-report 2) => (re #"Actual.*2")
    (nth raw-report 3) => (re #"Checking function.*test-checker 33")
    (nth raw-report 4) => (re #"intermediate values")
    (nth raw-report 5) => (re #"\(f 1\) => 33"))
    
  (let [failure-map {:type :mock-expected-result-functional-failure
		     :actual 2
		     :position ["foo.clj" 3]
		     :expected 'odd?}
	raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => (re #"FAIL.*foo.clj:3")
    (nth raw-report 1) => (re #"Actual.*did not agree")
    (nth raw-report 2) => (re #"Actual.*2")
    (nth raw-report 3) => (re #"Checking function.*odd?"))

  "values in strings are formatted via pr-str"
  (let [failure-map {:type :mock-expected-result-functional-failure
	       :actual nil
	       :expected '(sloobom "forp")
	       :intermediate-results [['(+ 1 "ate") nil]]
	       :position ["foo" 23]}
	raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 2) => (re #"result: nil")
    (nth raw-report 3) => (re #"function: \(sloobom \"forp\"")
    (nth raw-report 5) => (re #"\(\+ 1 \"ate\"\) => nil")))


(fact "excess matches"
  (let [failure-map {:type :mock-argument-match-failure
		     :actual '(nil)
		     :position ["foo.clj" 3]
		     :function odd?}
	raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => (re #"FAIL at .*foo.clj:3")
    (nth raw-report 1) => (re #"never said odd\? would be needed")
    (nth raw-report 2) => "(nil)"))

(fact "mock called an incorrect number of times"
  (let [failure-map {:type :mock-incorrect-call-count
		     :position ["foo.clj" 3]
		     :expected "(f a)"}
	raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => (re #"FAIL at .*foo.clj:3")
    (nth raw-report 1) => (re #"claimed the following was needed")
    (nth raw-report 2) => "(f a)"))

(fact "ordinary bad result from equality"
  (let [failure-map {:type :mock-expected-result-failure
		     :position ["foo.clj" 3]
		     :actual nil
		     :expected "s"}
	raw-report (with-identity-renderer (clojure.test/old-report failure-map))]
    (nth raw-report 0) => (re #"FAIL at .*foo.clj:3")
    (nth raw-report 1) => (re #"Expected: \"s\"")
    (nth raw-report 2) => (re #"Actual: nil")))
