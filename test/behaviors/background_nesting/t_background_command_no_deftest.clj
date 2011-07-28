;; -*- indent-tabs-mode: nil -*-

(ns behaviors.background_nesting.t-background-command-no-deftest
  (:use clojure.test)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use clojure.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(background (outermost ...o...) => 1)
(fact (+ 1 (outermost ...o...)) => 2)
(background (outermost ...o...) => -1)
(fact (+ 1 (outermost ...o...)) => 0)

