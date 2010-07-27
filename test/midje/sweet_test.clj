(ns midje.sweet-test
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util]))


(deftest simple-assertion-examples
  (after (fact (+ 1 1) => 3)
	 (is (last-type? :mock-expected-result-failure)))

  (after 
   (facts (+ 10 10) => 20
	  (+ 20 20) => 40)
   (is (no-failures?)))
)


(only-mocked g)
(defn f [n] (g n))

(deftest simple-mocking-examples
(comment
  (after
   (fact (f 1) => 33
      (provided (g 1) => 33))
   (is (no-failures?)))
)
)
