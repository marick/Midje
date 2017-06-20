(ns as-documentation.prerequisites.fact-wide
  (:require [midje.sweet :refer :all]))

;;; Here is a simple motivating example. Suppose you have a predicate `pilot-ready?` that
;;; depends on other predicates.

(unfinished pilot-ready? copilot-ready? engines-ready?)

;; It would be annoying to specify the values of each predicate for each checkable. You'd
;; rather define some defaults for the whole fact and override the ones that change for each
;; checkable. Here's how that's done.

;; Note that the var references are required if Midje is to override them.
(def flight-ready? (every-pred #'pilot-ready? #'copilot-ready? #'engines-ready?))

(fact
  (prerequisites (pilot-ready? ..flight..) => true
                 (copilot-ready? ..flight..) => true
                 (engines-ready? ..flight..) => true)

  (flight-ready? ..flight..) => truthy
  (flight-ready? ..flight..) => falsey (provided (pilot-ready? ..flight..) => false)
  (flight-ready? ..flight..) => falsey (provided (copilot-ready? ..flight..) => false)
  (flight-ready? ..flight..) => falsey (provided (engines-ready? ..flight..) => false))


;;; Interactions between prerequisites

(unfinished x-handler y-handler)

(defn function-under-test [x y]
  (+ (x-handler x) (y-handler y)))

(fact "in case of multiple possible matches, the latest counts"
  (prerequisites (x-handler anything) => -100
                 (x-handler 1) => 1
                 (y-handler 2) => 22)
  (fact "matches both `x-handler` prerequisites"
    (function-under-test 1 2) => (+ 1 22))
  (fact "matches only the earlier variant"
    (function-under-test 888 2) => (+ -100 22))
  (fact "a `provided` prerequisite always wins"
    (function-under-test 1 2) => (+ 8 9)
    (provided
      (x-handler anything) => 8
      (y-handler 2) => 9)))


;; Examples from the documentation

(fact "prerequisites can be nested"
  (prerequisite (x-handler 1) => 8000)
  (fact
    (prerequisite (y-handler 1) => 80)
    (function-under-test 1 1) => 8080)

  (fact
    (prerequisite (y-handler 1) => -8000)
    (function-under-test 1 1) => 0))

(fact "prerequisites can be nested"
  (prerequisites (x-handler 1) => 10
                 (y-handler 1) => 8)
  (fact
    (prerequisite (y-handler 1) => 33)

    (function-under-test 1 1) => (+ 10 33)

    (function-under-test 1 1) => (+ 10 99)
    (provided
      (y-handler 1) => 99)))

(fact "catch-all or default prerequisites"
  (prerequisites (x-handler anything) => 1
                 (y-handler anything) => 2
                 (y-handler 3) => 333)
  (function-under-test 1 1) => (+ 1 2)
  (function-under-test 1 3) => (+ 1 333))

(fact "note that it's the most recent prerequisite, *not* the most specific"
  (prerequisites (x-handler anything) => 1
                 (y-handler 3) => 3
                 (y-handler anything) => 222)
  (function-under-test 1 1) => (+ 1 222)
  (function-under-test 1 3) => (+ 1 222))


(let [my-favorite-argument-value 1
      my-favorite-expected-value 32000]
  (fact "lexical scoping is obeyed"
    (prerequisites (x-handler my-favorite-argument-value) => my-favorite-expected-value
                   (y-handler my-favorite-argument-value) => my-favorite-expected-value)

    (function-under-test my-favorite-argument-value my-favorite-argument-value)
    => (* 2 my-favorite-expected-value)))
