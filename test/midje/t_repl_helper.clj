(ns midje.t-repl-helper
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

(fact "a simple test"
  (+ 1 2) => 3)

(fact "a non-featherian test" :non-featherian
  (cons 1 nil) => [1])
