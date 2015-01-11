(ns midje.util.t-one-seven
  (:use [midje.sweet]
        [midje.util laziness thread-safe-var-nesting]
        [midje.test-util]))


(unfinished exploder)

;; The following is lazy, so it should not cause an error.
(map exploder [1 2 3])

(defrecord Foo [x y])
