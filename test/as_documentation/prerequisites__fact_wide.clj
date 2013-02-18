(ns as-documentation.prerequisites--fact-wide
  (:use midje.sweet))

(unfinished x-handler y-handler)

(defn function-under-test [x y]
  (+ (x-handler x) (y-handler y)))


;; The following shows how prerequisites can be
;; applied to a whole fact rather than just to
;; one prediction.

(fact "fact-wide prerequisites"
  (prerequisites (x-handler anything) => -1
                 (x-handler 1) => 1
                 (y-handler 1) => 2)
  
  (function-under-test 1 1) => 3
  (function-under-test 8 1) => 1)


;; You can override fact-wide prerequisites in
;; individual `provided` clauses.

(fact "fact-wide prerequisites can be overridden"
  (prerequisites (x-handler anything) => -1
                 (x-handler 1) => 1
                 (y-handler 1) => 2)
  
  (function-under-test 1 1) => 35
  (provided
    (y-handler 1) => 34)
  (function-under-test 8 1) => 35
  (provided
    (x-handler 8) => 33))


;; Prerequisites apply to nested facts.

(facts "prerequisites apply to nested facts."
  (prerequisites (x-handler anything) => -1
                 (x-handler 1) => 1
                 (y-handler 1) => 2)

  (fact "a nested fact"
    (function-under-test 1 1) => 3)

  (fact "a nested fact with its own prediction-specific prerequisite"
    (function-under-test 8 1) => 35
    (provided
      (x-handler 8) => 33)))
