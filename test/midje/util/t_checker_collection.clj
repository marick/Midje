(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))


(fact "sequentials that are to contain things"
  [1 2 3] => (contains [1 2])  
  [1 2 3] => (contains [2 3])
  [1 2 3] => (contains [even? odd?])
  
  ( (contains [3 1]) [1 2 3]) => falsey ; order matters by default.
  ( (contains [1 3]) [1 2 3]) => falsey ; gaps are disallowed by default

  "the desire for order can be controlled"
  [1 2 3] => (contains [3 1] :in-any-order)
  ( (contains [1 3] :in-any-order) [1 2 3]) => falsey ; gaps are still disallowed
)
