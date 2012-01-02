;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-util
  (:use midje.sweet
        [midje.checkers util]
        midje.test-util))

(defrecord R [a])

(fact "captured exceptions can be recognized"
  (captured-throwable? (captured-throwable (Throwable.))) => truthy
  "and are not fooled by maps or records"
  (captured-throwable? {}) => falsey
  (captured-throwable? (sorted-map :a 3)) => falsey
  (captured-throwable? (R. 1)) => falsey)

