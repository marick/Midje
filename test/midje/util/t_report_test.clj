(ns midje.util.t-report-test
  (:use [midje.util.report] :reload-all)
  (:use [clojure.test])
  (:use [midje.test-util]))


(testable-privates midje.util.report midje-position-string functional-failure-lines
		   without-nasty-looking-functions)

(deftest position-string-test
  (is (= (midje-position-string ["filename.clj" 33])
	 "(filename.clj:33)")))


(deftest flatten-and-remove-nils-test
  (is (= [] (flatten-and-remove-nils '())))
  (is (= ["foo" "bar" "baz"]
	   (flatten-and-remove-nils '(nil "foo" ("bar" nil "baz"))))))

(deftest functional-failure-renderer-test
  (let [failure-map {:type :mock-expected-result-functional-failure
		     :actual 2
		     :actual-processor #'inc
		     :processed-actual 3
		     :position ["foo.clj" 3]
		     :expected '(test-checker 33)}
	raw-report (with-identity-renderer (render failure-map))]
    (is (re-find #"FAIL.*foo.clj:3" (nth raw-report 0)))
    (is (re-find #"Actual.*did not agree" (nth raw-report 1)))
    (is (re-find #"Actual.*2" (nth raw-report 2)))
    (is (re-find #"Checking function.*test-checker 33" (nth raw-report 3)))
    (is (re-find #"applied.*#'inc" (nth raw-report 4)))
    (is (re-find #"surprised.*3" (nth raw-report 5))))
    
  (let [failure-map {:type :mock-expected-result-functional-failure
		     :actual 2
		     :position ["foo.clj" 3]
		     :expected 'odd?}
	raw-report (with-identity-renderer (render failure-map))]
    (is (re-find #"FAIL.*foo.clj:3" (nth raw-report 0)))
    (is (re-find #"Actual.*did not agree" (nth raw-report 1)))
    (is (re-find #"Actual.*2" (nth raw-report 2)))
    (is (re-find #"Checking function.*odd?" (nth raw-report 3)))))

;; In the case of the checker (exactly odd?), you want to see failures
;; written in terms of a function name instead of some absurdly complicated
;; #<core$even_QMARK_ clojure.core$even_QMARK_@15ee9cc3>

(deftest without-nasty-looking-functions-test
  (is (= "a function named 'even?'" (without-nasty-looking-functions even?)))
  (is (re-find #"fn__" (str (without-nasty-looking-functions (fn [n] 1))))))

