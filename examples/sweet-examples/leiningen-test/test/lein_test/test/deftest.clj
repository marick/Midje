(ns lein-test.test.deftest
  (:use [lein-test.core] :reload)
  (:use [midje.sweet]
	[clojure.test]))

(deftest one-fact-in-a-test
  (fact (+ 1 1) => 3))

(deftest two-facts-in-a-test
  (fact (+ 1 2) => 4)
  (fact (+ 2 3) => 4))
