(ns behaviors.background-nesting.t-shadowing-outside-background
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(background (outermost) => 2
            (middlemost) => 'a)

(deftest background-command-is-shadowed-by-against-background   ; deftest intentional
  (against-background [ (middlemost) => 33]
    (fact (+ (middlemost) (outermost)) => 35)))

(against-background [ (middlemost) => 33]
  (fact (+ (middlemost) (outermost)) => 35))

