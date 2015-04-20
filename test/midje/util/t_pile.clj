(ns midje.util.t-pile
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:require [midje.util.pile :as subject]))

(facts "you can tack new keys onto a hashmap"
  (subject/tack-on-to {:a [1], :b [55] :c 'blah} :a 2 :b 56) => {:a [1 2], :b [55 56], :c 'blah})

(fact "map-difference"
  (subject/map-difference {:a 1, :b 2} {:a 1, :c 3}) => {:b 2})

  
(fact "apply each function to each corresponding arg" 
  (subject/apply-pairwise [inc dec] [1 1] [2 2]) => [[2 0] [3 1]])

(fact 
  (subject/map-first str [1 2 3]) => ["1" 2 3])

(fact "sort a map"
  (subject/sort-map {:z 26 :b 2 :a 1}) => {:a 1 :b 2 :z 26})



(fact "any-pred-from"
  ((subject/any-pred-from [odd? even?]) 1) => true
  ((subject/any-pred-from [pos? neg?]) 0) => false
  ((subject/any-pred-from [:key :word]) {:key false}) => false
  ((subject/any-pred-from [:key :word]) {:key false :word 3}) => true
  ((subject/any-pred-from [#{1 2} #{3 4}]) 3) => true
  ;; stops at first match
  ((subject/any-pred-from [(partial = 3) (fn[_](throw (new Error "boom!")))]) 3) => true
  ;; Any empty list means that everything matches
  ((subject/any-pred-from []) 3) => true)

(fact "stringlike-matches?"
  (subject/stringlike-matches? "foo" "ofoop") => true
  (subject/stringlike-matches? "foo" "ooop") => false
  (subject/stringlike-matches? "foo" nil) => false
  (subject/stringlike-matches? "foo" [1 2 3]) => false
  (subject/stringlike-matches? #"fo." "ofop") => true
  (subject/stringlike-matches? #"fo." "ooop") => false
  (subject/stringlike-matches? #"fo." false) => false)

