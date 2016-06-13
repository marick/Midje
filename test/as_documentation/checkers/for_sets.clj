(ns as-documentation.checkers.for-sets
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))


(fact "`just` provides extended equality to set equality"
  #{3 8 1} => (just odd? 3 even?))

(fact "`contains` works with subsets"
  (fact "subsets of literal values"
    #{1 2 3} => (contains 3))
  (fact "subsets of checkers"
    #{1 2 3} => (contains odd? even?)))


(fact "checking properties of elements"
  (fact "a known number"
    #{1 3 5} => (three-of odd?))
  (fact "number irrelevant"
    #{1 3 5} => (has every? odd?)))

