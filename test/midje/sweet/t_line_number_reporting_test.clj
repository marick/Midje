(ns midje.sweet.t-line-number-reporting-test
  (:use [midje.sweet] :reload-all)
  (:use [clojure.test])
  (:use [midje.test-util]))


(defn f [n] n)

(def position-1 9)
(deftest expected-result-positions-test
  (after 
   (fact (+ 1 1) => 3)
   (is (reported? 1 [{:type :mock-expected-result-failure
		      :position ["t_line_number_reporting_test.clj" (+ position-1 3)]}])))

  (after 
   (fact
    (+ 1 1) => 3)
   (is (reported? 1 [{:type :mock-expected-result-failure
		      :position ["t_line_number_reporting_test.clj" (+ position-1 9)]}])))

    (after 
     (fact
      
     (+ 1 1) =>

         3)
     (is (reported? 1 [{:type :mock-expected-result-failure
			:position ["t_line_number_reporting_test.clj" (+ position-1 16)]}]))))

(defn g [n] n)
(def position-2 32)
(deftest partial-call-test
  (after 
   (fact (g 1) => 1
         (provided (f 2) => 2))
   (is (reported? 2 [{:type :mock-incorrect-call-count
		      :position ["t_line_number_reporting_test.clj" (+ position-2 4)]}
		     {:type :pass}])))

  (after 
   (fact (g 1) => 1


	 (provided
	    (f 2) => 2))
   (is (reported? 2 [{:type :mock-incorrect-call-count
		      :position ["t_line_number_reporting_test.clj" (+ position-2 14)]}
		     {:type :pass}]))))

(unfinished favorite-animal)
(defn favorite-animal-name [] (name (favorite-animal)))
(defn favorite-animal-empty [] )
(defn favorite-animal-only-animal [] (favorite-animal))
(defn favorite-animal-only-name [] (name "fred"))
(defn favorite-animal-one-call [] (name (favorite-animal 1)))

(deftest unfolding-expectations-examples
  (after
   (fact
     (favorite-animal-name) => "betsy"
     (provided
       (name (favorite-animal)) => "betsy"))
   (is (no-failures?)))

  (def line-number 66)
  (after
   (fact
     (favorite-animal-empty) => "betsy"
     (provided
       (name (favorite-animal)) => "betsy"))
   (is (reported? 3 [ {:type :mock-incorrect-call-count
		       :expected-call "(favorite-animal)"
		       :position ["t_line_number_reporting_test.clj" (+ line-number 5)]}
		      {:type :mock-incorrect-call-count
		       :position ["t_line_number_reporting_test.clj" (+ line-number 5)]
		       :expected-call "(name ...favorite-animal-value-1...)" }
		      {:type :mock-expected-result-failure
		       :position ["t_line_number_reporting_test.clj" (+ line-number 3)]}])))
		       
  (after
   (fact
     (favorite-animal-only-animal) => "betsy"
     (provided
       (name (favorite-animal)) => "betsy"))
   (is (reported? 2 [{:type :mock-incorrect-call-count}
		     {:type :mock-expected-result-failure}])))

  (after
   (fact
     (favorite-animal-only-name) => "betsy"
     (provided
       (name (favorite-animal)) => "betsy"))
   (is (reported? 4 [{:type :mock-argument-match-failure
		      :function #'clojure.core/name
		      :actual '("fred")}
		     {:type :mock-incorrect-call-count
		      :expected-call "(favorite-animal)"}
		     {:type :mock-incorrect-call-count
		      :expected-call "(name ...favorite-animal-value-1...)"}
		     {:type :mock-expected-result-failure}])))


  (after
   (fact
     (favorite-animal-one-call) => "betsy"
     (provided
       (name (favorite-animal 1)) => "betsy"
       (name (favorite-animal 2)) => "jake"))
   (is (reported? 3 [{:type :mock-incorrect-call-count
		      :expected-call "(favorite-animal 2)"}
		     {:type :mock-incorrect-call-count
		      :expected-call "(name ...favorite-animal-value-2...)"}
		     {:type :pass}])))
)

(unfinished outermost middlemost innermost)
(deftest line-numbers-for-background-facts
  (in-separate-namespace
   (background (outermost) => 2)
   (against-background [ (middlemost) => 33]
     (after
      (fact
	(against-background (innermost) => 8)
	(+ (middlemost)
	   (outermost)
	   (innermost)) => 44
	   (+ 1 (middlemost)) => 2)
      (is (reported? 2 [{ :position ["t_line_number_reporting_test.clj" 125]}
  		        { :position ["t_line_number_reporting_test.clj" 128]}] ))))))
