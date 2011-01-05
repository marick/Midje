;; -*- indent-tabs-mode: nil -*-

(ns behaviors.background-nesting.t-shadowing-outside-background
  (:use clojure.test)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(background (outermost) => 2
            (middlemost) => 'a)

(deftest background-command-is-shadowed-by-against-background   ; deftest intentional
  (against-background [ (middlemost) => 33]
    (fact (+ (middlemost) (outermost)) => 35)))
  
(against-background [ (middlemost) => 33]
  (fact (+ (middlemost) (outermost)) => 35))
  
