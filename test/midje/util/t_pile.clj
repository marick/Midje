(ns midje.util.t-pile
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.util.pile :as subject]))

(fact "apply each function to each corresponding arg"
  (subject/apply-pairwise [inc dec] [1 1] [2 2]) => [[2 0] [3 1]])

(fact
  (subject/map-first str [1 2 3]) => ["1" 2 3])

(fact "sort a map"
  (subject/sort-map {:z 26 :b 2 :a 1}) => {:a 1 :b 2 :z 26})


