(ns behaviors.background_nesting.t-background-command
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(deftest background-command-slams-new-background-in-place  ; deftest intentional
  (background (outermost ...o...) => 1)
  (fact (+ 1 (outermost ...o...)) => 2)
  (background (outermost ...o...) => -1)
  (fact (+ 1 (outermost ...o...)) => 0))

