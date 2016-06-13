(ns behaviors.background-nesting.t-left-to-right
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)
      
(deftest left-to-right-shadowing        ; deftest intentional
  (against-background [ (middlemost) => 33 (middlemost) => 12]
    (fact (* 2 (middlemost)) => 24)))

(against-background [ (middlemost) => 33 (middlemost) => 12]
  (fact (* 2 (middlemost)) => 24))

