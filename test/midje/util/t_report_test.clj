(ns midje.util.t-report-test
  (:use [midje.util.report] :reload-all)
  (:use [clojure.test])
  (:use [midje.test-util]))


(testable-privates midje.util.report midje-position-string)

(deftest position-string-test
  (is (= (midje-position-string ["filename.clj" 33])
	 "(filename.clj:33)")))

