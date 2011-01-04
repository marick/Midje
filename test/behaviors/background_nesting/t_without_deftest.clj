;; -*- indent-tabs-mode: nil -*-

(ns behaviors.background_nesting.t-without-deftest
  (:use clojure.test)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(against-background [ (middlemost ...one...) => 1 ]
  (let [two 2]
    (facts
      (vector? [1 two]) => truthy
      (let [three 3]
        (+ (middlemost ...one...) two three) => 6))))
