;; -*- indent-tabs-mode: nil -*-

(ns midje.t-checkers
  (:use midje.sweet
        midje.test-util))

;; Make sure the published interface actually exists.

(fact "simple checkers exist"
  1 => truthy
  nil => falsey
  'foo => anything
  odd? => (exactly odd?)
  (throw (Error.)) => (throws Error))

(fact "deprecated checkers"
  [1 2 3] => (in-any-order [3 2 1])
  {:a 1 :b 2} => (map-containing {:a 1})
  [{:a 1} {:b 2}] => (maps-containing {:a 1} {:b 2})
  [{:a 1} {:b 2}] => (only-maps-containing {:a 1} {:b 2}))

(defn equality [expected]
  (chatty-checker [actual] (= actual expected)))

(fact "chatty checkers exist"
  1 => (equality 1))

(fact "collection checkers"
  [1] => (contains 1)
  [1] => (just 1)
  [1] => (has-prefix 1)
  [1] => (has-suffix 1)
  [1] => (has every? odd?))
    

  

