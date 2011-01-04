;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-laziness
  (:use [midje.sweet]
        [midje.util laziness]
        [midje.test-util]))

;; Justification for use of eagerly
(def counter (atom 1))
(def mocked-function-produces-next-element inc)

(defn function-under-test-produces-a-lazy-list []
  (iterate mocked-function-produces-next-element 1))

(defn mock-use []
  (binding [mocked-function-produces-next-element (fn [n] (swap! counter inc) (inc n))]
    (eagerly (take 5 (function-under-test-produces-a-lazy-list)))))

(fact "eagerly forces evaluation"
  (mock-use)
  @counter => 5)

(fact "eagerly allows non-sequences"
  (eagerly 3) => 3)


