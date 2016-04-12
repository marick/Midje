(ns behaviors.background_nesting.t-without-deftest
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(against-background [ (middlemost ...one...) => 1 ]
  (let [two 2]
    (facts
      (vector? [1 two]) => truthy
      (let [three 3]
        (+ (middlemost ...one...) two three) => 6))))
