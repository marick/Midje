(ns implementation.emission.plugins.fim-flare (:require [midje.emission.plugins.flare :as subject])
    (:use midje.sweet midje.test-util midje.util)
    (:require [flare.core :as core]))

(def f #(subject/generate-reports (core/diff %1 %2)))

(fact "representing differences in maps"
  (future-fact
    (f '{:a a :b b} '{:a Z :b b})
    => ["expected `a`, got `Z` at {:a % ...}"])    ; "in [:a] expected: a, was Z"

  (future-fact
    (f '{:a a :b b :c c} '{:a Z :b b :c Y})
    => ["Expected `a`, got `Z` at {:a % ...}"      ; "in [:a] expected: a, was Z"
        "Expected `a`, got `Z` at {:c % ...}"])    ; "in [:c] expected: c, was Y"

  (future-fact "extra actual elements"
    (f '{:a a :b b} '{:a a :b b :c c})
    => ["Actual has extra key `:c` at {:c % ...}"] ; "map contained key: :c, but not expected."

    (f '{:a a :b b} '{:a a :b b :c c :d d})
    => ["Actual has extra keys `:c` and `:d` at {:c % ...}"]) ; use cl-format

  (future-fact "Missing actual elements"
    (f '{:a a :b b} '{:a a})
    => ["Actual is missing key `:b` at {...}"]

    (f '{:a a :b b} '{})
    => ["Actual is missing keys `:a` and `:b` at {...}"]) ; alphabetic if sortable

  (future-fact "maps within maps"
    (f '{:a {:a a}} '{:a {:a z}})
    => ["expected `a`, got `Z` at {:a {:a %...}}"]

    (f '{1 {:a a :b b}} '{1 {:a a :b b :c c}})
    => ["Actual has extra key `:c` at {1 {:c % ...}}"]

    (f '{1 {:a a :b b}} '{1 {:a a}})
    => ["Actual is missing key `:b` at {1 {...}}"])
)

(fact "representating differences in sequentials"
  (future-fact "indexes are highlighted with `%`"
    (f '[a] '[Z])
    => [       "Expected `a`, got `Z` at [%0..."])
        ;; vs. "in [0] expected: a, was Z"

  (future-fact "use the `don't care about value` symbol for elements 1 and 2"
    (f '[a b c] '[a Z Y])
    => [   "Expected `b`, got `Z` at [_ %1..."
        ;; "in [1] expected: b, was Z"
           "Expected `c`, got `Y` at [_ _ %2..."])
        ;; "in [2] expected: c, was Y"

  (future-fact "use ellipses for elements further than 2"
    (f '[a b c d] '[a b c Z])
    => [   "Expected `d`, got `Y` at [... %3..."])
        ;; "in [3] expected: d, was Y"

  (future-fact "note that entire differing element is printed"
    (f '[a b c] '[a [Z z zz]])
    => ["Expected `b`, got `[Z z zz]` at [_ %1..."])

  (future-fact "Actual has extra elements; location is last matching element" 
    (f '[a b] '[a b c d e])
    => [  "Actual has 3 extra elements `(c d e)` at [_ _ %2..."]) 
        ; "expected length of sequence is 2, actual length is 5.\nactual has 3 elements in excess: (c d e)"

  (future-fact "Actual has too few; location is one past end of actual"
    (f '[a b c d e f g] '[a b c d e])
    => [   "Actual is missing `f` and 1 other element at [... %5...]"])
        ;; "expected length of sequence is 7, actual length is 5.\nactual is missing 2 elements: (f g)"

  (future-fact "sequentials within sequentials"
    (f '[1 [2 3]   [4 5] [6 7] 8]
       '[1 [2 3 X] [4  ] [6 Y] Z])
    => ["Actual has 1 extra element `(X)` at [_ %1[_ %1...]]
)



;; (fact "mixtures"
;;   (future-fact
;;     (f '{:a [a b] :b b} '{:a [a Z] :b b})
;;     => ["At [%0 {:a % ...} ...}, expected `a`, actual `Z`"])    ; "in [:a] expected: a, was Z"
;; )
