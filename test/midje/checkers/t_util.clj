;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-util
  (:use midje.sweet
        [midje.checkers util]
        midje.test-util))

(defrecord R [a])

(fact "captured exceptions can be recognized"
  (captured-exception? (captured-exception (Throwable.))) => truthy
  "and are not fooled by maps or records"
  (captured-exception? {}) => falsey
  (captured-exception? (sorted-map :a 3)) => falsey
  (captured-exception? (R. 1)) => falsey)

