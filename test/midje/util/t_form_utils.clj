(ns midje.util.t-form-utils
  (:use [midje.util.form-utils])
  (:use [midje.sweet])
  (:use [midje.test-util]))

(facts "you can tack new keys onto a hashmap"
  (tack-on-to {:a [1], :b [55] :c 'blah} :a 2 :b 56) => {:a [1 2], :b [55 56], :c 'blah})

(fact "map-difference"
  (map-difference {:a 1, :b 2} {:a 1, :c 3}) => {:b 2})

  
(fact "apply each function to each corresponding arg" 
  (apply-pairwise [inc dec] [1 1] [2 2]) => [[2 0] [3 1]])


(fact "sort a map"
  (sort-map {:z 26 :b 2 :a 1}) => {:a 1 :b 2 :z 26})


(fact "any-pred-from"
  ((any-pred-from [odd? even?]) 1) => true
  ((any-pred-from [pos? neg?]) 0) => false
  ((any-pred-from [:key :word]) {:key false}) => false
  ((any-pred-from [:key :word]) {:key false :word 3}) => true
  ((any-pred-from [#{1 2} #{3 4}]) 3) => true
  ;; stops at first match
  ((any-pred-from [(partial = 3) (fn[_](throw (new Error "boom!")))]) 3) => true
  ;; Any empty list means that everything matches
  ((any-pred-from []) 3) => true)

