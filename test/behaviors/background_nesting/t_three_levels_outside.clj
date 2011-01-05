;; -*- indent-tabs-mode: nil -*-

(ns behaviors.background-nesting.t-three-levels-outside
  (:use clojure.test)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
  )

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(background (outermost) => 2
            (middlemost) => 'a)

(deftest three-levels-of-nesting-one-duplicated         ; deftest intentional
  (against-background [ (middlemost 2) => 33
                        (innermost) => 'c]

    (against-background [ (middlemost 1) => -43 ]
      (fact
        (against-background (innermost) => 8)
        (+ (middlemost 2) (middlemost 1) (outermost) (innermost)) => 0))))


(against-background [ (middlemost 2) => 33
                      (innermost) => 'c]
  
  (against-background [ (middlemost 1) => -43 ]
    (fact
      (against-background (innermost) => 8)
      (+ (middlemost 2) (middlemost 1) (outermost) (innermost)) => 0)))

