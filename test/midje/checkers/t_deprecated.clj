;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.deprecated
  (:use midje.sweet
        midje.checkers.deprecated
        midje.test-util))


(facts "about in-any-order"
  [] => (in-any-order [])
  [1] => (in-any-order [1])
  '(2 1) => (in-any-order [1 2])
  [ {:a 1} {:b 2} ] => (in-any-order [{:b 2} {:a 1}])

  ( (in-any-order [1 2]) [1 2 3]) => falsey
  ( (in-any-order [1 2]) [1]) => falsey
  ( (in-any-order [1 2]) [1 3]) => falsey
  
  ( (in-any-order [1 2 2 3]) [1 2 3 3]) => falsey
  ( (in-any-order [2 1 3 2]) [1 2 3 3]) => falsey)

(facts "about map-containing"
  {:a 1 :b 2} => (map-containing {:a 1 :b 2})
  {:a 1 :b 2 :c 3} => (map-containing {:a 1 :b 2})

  ( (map-containing {:a 1 :b 2})  {:a 1}) => falsey
  ( (map-containing {:a 1 :b 2})  {:a 1 :b 3}) => falsey)

(facts "about only-maps-containing-test"
  ( (only-maps-containing {:a 1 :b 2}) [{:a 1 :b 2} {:extra true}]) => falsey
  ( (only-maps-containing {:a 1 :b 2}  {:extra true}) [{:a 1 :b 2}]) => falsey

  [{:a 1 :b 2} {:extra 1}] => (only-maps-containing {:extra 1} {:a 1})
  [{:a 1 :b 2} {:a 1 :b 22}] => (only-maps-containing {:b 2} {:b 22})
  [{:a 1 :b 2} {:a 1 :b 22}] => (only-maps-containing [{:b 2} {:b 22}])
  ( (only-maps-containing {:b 2} {:b 22}) [{:b 2} {:b 33}]) => falsey)

(facts "about maps-containing"
  ( (maps-containing {:a 1 :b 2}  {:extra true}) [{:a 1 :b 2}]) => falsey

  [{:a 1 :b 2} {:extra 1}] => (maps-containing {:extra 1} {:a 1})
  [{:a 1 :b 2} {:a 1 :b 22}] => (maps-containing {:b 2} {:b 22})
  [{:a 1 :b 2} {:a 1 :b 22} {:a 1 :b 33}] => (maps-containing {:b 2} {:b 22})
  [{:a 1 :b 2} {:a 1 :b 22} {:a 1 :b 33}] => (maps-containing [{:b 2} {:b 22}])
  ( (maps-containing {:b 2} {:b 22}) [{:b 2} {:b 33}]) => falsey)



