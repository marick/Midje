(ns midje.util.t-laziness
  (:use [midje.sweet]
        [midje.util laziness thread-safe-var-nesting]
        [midje.test-util]))

;; Justification for use of eagerly
(def counter (atom :needs-to-be-initialized))
(def #^:dynamic *mocked-function-produces-next-element* inc)

(defn function-under-test-produces-a-lazy-list []
  (iterate *mocked-function-produces-next-element* 1))

(defn mock-use []
  (binding [*mocked-function-produces-next-element* (fn [n] (swap! counter inc) (inc n))]
    (eagerly (take 5 (function-under-test-produces-a-lazy-list)))))

(fact "eagerly forces evaluation"
  (reset! counter 1)
  (mock-use)
  @counter => 5)

;; After justification, more facts.

(unfinished exploder)

;; The following is lazy, so it should not cause an error.
(map exploder [1 2 3])

(defrecord Foo [x y])
