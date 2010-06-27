(ns midje.report-test
  (:use [midje.report] :reload-all)
  (:use [clojure.test])
  (:use [midje.test-util]))


(testable-privates midje.report midje-position-string)

(deftest position-string-test
  (is (= (midje-position-string ["filename.clj" 33])
	 "(filename.clj:33)")))

