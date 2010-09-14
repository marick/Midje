(ns midje.sweet.t-line-number-reporting-test
  (:use [midje.sweet] :reload-all)
  (:use [clojure.test])
  (:use [midje.test-util]))


(defn f [n] n)

(def position-1 9)
(deftest expected-result-positions-test
  (after 
   (fact (+ 1 1) => 3)
   (is (last-type? :mock-expected-result-failure))
   (is (last-file? "t_line_number_reporting_test.clj"))
   (is (last-line? (+ position-1 3))))

  (after 
   (fact
    (+ 1 1) => 3)
   (is (last-type? :mock-expected-result-failure))
   (is (last-file? "t_line_number_reporting_test.clj"))
   (is (last-line? (+ position-1 10))))

    (after 
     (fact
      
     (+ 1 1) =>

         3)
   (is (last-type? :mock-expected-result-failure))
   (is (last-file? "t_line_number_reporting_test.clj"))
   (is (last-line? (+ position-1 18)))))

(defn g [n] n)
(def position-2 35)
(deftest partial-call-test
  (after 
   (fact (g 1) => 1
         (provided (f 2) => 2))
   (is (last-type? :mock-incorrect-call-count))
   (is (last-file? "t_line_number_reporting_test.clj"))
   (is (last-line? (+ position-2 4))))

  (after 
   (fact (g 1) => 1


	 (provided
	    (f 2) => 2))
   (is (last-type? :mock-incorrect-call-count))
   (is (last-file? "t_line_number_reporting_test.clj"))
   (is (last-line? (+ position-2 14)))))
