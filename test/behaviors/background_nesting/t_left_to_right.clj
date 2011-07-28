;; -*- indent-tabs-mode: nil -*-

(ns behaviors.background-nesting.t-left-to-right
  (:use clojure.test)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use clojure.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)
      
(deftest left-to-right-shadowing        ; deftest intentional
  (against-background [ (middlemost) => 33 (middlemost) => 12]
    (fact (* 2 (middlemost)) => 24)))

(against-background [ (middlemost) => 33 (middlemost) => 12]
  (fact (* 2 (middlemost)) => 24))

