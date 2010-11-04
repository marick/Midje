(ns midje.sweet.t-background
  (:use clojure.test)
  (:use [midje.sweet.background])
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util]))

(deftest trivial-case-test
  (after
   (against-background
    (fact (+ 1 1) => 3))
   (is (reported? 1 [{:type :mock-expected-result-failure}]))))
