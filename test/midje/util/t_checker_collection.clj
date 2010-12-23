(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))


(fact "sequentials that are to contain things"
  [3 4 5 700] => (contains [4 5 700])
  ( (contains [4 5 700]) [4 700 5]) => falsey
  ( (contains [4 5 700]) [4 5 'hi 700]) => falsey

  ['hi 700 5 4] => (contains [4 5 700] :in-any-order)
  ( (contains [4 5 700] :in-any-order) [4 5 'hi 700]) => falsey

  [4 5 'hi 700] => (contains [4 5 700] :gaps-ok)
  ( (contains [4 5 700] :gaps-ok) [4 700 'hi' 5]) => falsey

  [4 700 5] => (contains [4 5 700] :gaps-ok :in-any-order)
  [4 5 'hi 700] => (contains [4 5 700] :in-any-order :gaps-ok)
  [700 'hi 4 5 'hi] => (contains [4 5 700] :in-any-order :gaps-ok)

  ;; containing sets
  [700 4 5] => (contains #{4 5 700})
  [700 4 5] => (contains #{4 5 700} :in-any-order) ; redundant
  [700 [] 4 5] => (contains #{4 5 700} :gaps-ok)
  [700 [] 4 5] => (contains #{4 5 700} :gaps-ok :in-any-order) ; redundant


  ;; Just
  [1 2 3] => (just [1 2 3]) 
  ( (just [1 2 3 4]) [1 2 3]) => falsey 
  ( (just [1 2 3]) [1 2 3 4]) => falsey 

  [1 2 3] => (just [odd? even? odd?])

  [1 3 2] => (just [1 2 3] :in-any-order)
  [1 3 2] => (just #{1 2 3})
  [1 3 2] => (just [1 2 3] :gaps-ok :in-any-order)  ;; silly
  [1 3 2] => (just #{1 2 3} :gaps-ok)


  [1 2 3] => (has-prefix [1 2])
  ( (has-prefix [2 1]) [1 2 3]) => false
  [1 2 3] => (has-prefix [2 1] :in-any-order)
  [1 2 3] => (has-prefix #{2 1})
  [1 2 3] => (has-prefix #{2 1} :gaps-ok)   ; silly

  [1 2 3] => (has-suffix [even? odd?])
  ( (has-suffix [odd? even?]) [1 2 3]) => falsey
  [1 2 3] => (has-suffix [odd? even?] :in-any-order)
  [1 2 3] => (has-suffix #{even? odd?})
  [1 2 3] => (has-suffix #{odd? even?} :gaps-ok)   ; silly

  ;; Singletons
  [700 4 5] => (contains 4)
  [4] => (just 4)
  [4] => (has-prefix 4)
  [4] => (has-suffix 4)

  ;; A few oddity cases
  (println "MAKE THESE WORK")
;  ( (contains [:k :v]) {:k :v}) => falsey
;  ( (contains 1) [1 2]) => falsey

  [4 4 1] => (has some odd?)
  [1 3 5] => (has every? odd?)
  ( (has some odd?) [34 34 88]) => falsey
  ( (has every? odd?) [1 3 44]) => falsey
)

