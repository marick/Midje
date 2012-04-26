(ns midje.checkers.t-combining
  (:use midje.sweet
        [midje.checkers.defining :only [checker?]]
        midje.test-util))


(facts "about checker combinators" 
  (some-checker truthy falsey)  => checker?
  (every-checker truthy falsey) => checker?  
  
  3 =>     (every-checker truthy number?)
  3 =not=> (every-checker truthy falsey)
  3 =>     (some-checker truthy falsey)
  3 =not=> (some-checker falsey string?)
  
  "works with chatty checkers' data-laden-falsehoods"
  {:a 1} =not=> (every-checker (contains {:b 1}))
  {:a 1} =not=> (some-checker  (contains {:b 1})))
