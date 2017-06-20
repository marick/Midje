(ns implementation.emission.plugins.fim-flare (:require [midje.emission.plugins.flare :as subject])
    (:require [midje.sweet :refer :all]
              [midje.test-util :refer :all]
              [midje.util :refer :all]
              [flare.core :as core]))

(def f #(subject/generate-reports (core/diff %2 %1)))

;; Note: In all these tests, the actual is on the LEFT, expected on the right.
;; That's consistent with other Midje functions and Midje's actual => expected notation.
;;
;; In evaluating the following, note that both actual and expected will be printed before
;; the lines with the differences. Or just Expected?

(fact "representing differences in maps"
  (future-fact
    (f '{:a Z :b b}
       '{:a a :b b})
    => ["At key :a, got `Z` instead of `a`"])    ; "in [:a] expected: a, was Z"

  (future-fact "Note that keys are not quoted"
    (f '{:a Z, "b" Z, 3 Z, d Z, [1 2] Z}
       '{:a 1, "b" 1, 3 1, d 1, [1 2] 1})
    => ["At key :a, got `Z` instead of `1`"
        "At key "b", got `Z` instead of `1`"
        "At key 3, got `Z` instead of `1`"
        "At key d, got `Z` instead of `1`"
        "At key [1 2], got `Z` instead of `1`"])

  (future-fact
    (f '{:a Z :b b :c Y}
       '{:a a :b b :c c})
    => ["At key :a, got `Z` instead of `a`"      ; "in [:a] expected: a, was Z"
        "At key :c, got `Y` instead of `c`"])    ; "in [:c] expected: c, was Y"

  (future-fact "extra actual elements"
    (f '{:a a :b b :c c}
       '{:a a :b b})
    => ["Extra key `:c`"] ; "map contained key: :c, but not expected."

    (f '{:a a :b b :c c :d d}
       '{:a a :b b})
    => ["Extra keys `:c` and `:d`"]) ; use cl-format

  (future-fact "Missing actual elements"
    (f '{:a a}
       '{:a a :b b})
    => ["Missing key `:b`"]

    (f  '{}
        '{:a a :b b})
    => ["Missing keys `:a` and `:b`"]) ; alphabetic if sortable

  (future-fact "maps within maps"
    (f '{:a {:a Z}}
       '{:a {:a a}} )
    => ["At path [:a :a], got `Z` instead of `a`"]

    (f '{1 {:a a :b b :c c}}
       '{1 {:a a :b b}})
    => ["At path [1], value has extra key `:c`"]

    (f '{1 {}}
       '{1 {:a a :b b}})
    => ["At path [1], value has missing keys :a and `:b`"])
)

(fact "representating differences in sequentials"
  (future-fact "The index is given instead of a key"
    (f '[Z]
       '[a])
    => [       "At index 0, got `Z` instead of `a`"])
        ;; vs. "in [0] expected: a, was Z"

  (future-fact "use the `don't care about value` symbol for elements 1 and 2"
    (f '[a Z Y]
       '[a b c])
    => [   "At index 1, got `Z` instead of `b`"
        ;; "in [1] expected: b, was Z"
           "At index 2, got `Y` instead of `c`"])
        ;; "in [2] expected: c, was Y"

  (future-fact "note that entire differing element is printed"
    (f '[a [Z z zz] ]
       '[a b        ] )
    => ["At index 1, got `[Z z zz] instead of `b`"])

  (future-fact "Actual has extra elements"
    (f '[a b c d e]
       '[a b      ])
    => [  "Actual has 3 extra elements `(c d e)`"])
        ; "expected length of sequence is 2, actual length is 5.\nactual has 3 elements in excess: (c d e)"

  (future-fact "Actual has too few"
    (f '[a b c d e    ]
       '[a b c d e f g])
    => [   "Actual is missing tail beginning with `f` (index 5)"])
        ;; "expected length of sequence is 7, actual length is 5.\nactual is missing 2 elements: (f g)"

  (future-fact "sequentials within sequentials"
    (f '[1 [2 3 X] [4  ] [6 Y] Z]
       '[1 [2 3]   [4 5] [6 7] 8])
    => ["At index [0 1], actual has 1 extra element `(X)`"
        "At index [0 2], actual is missing tail beginning with `5` (index 1)"
        "At index [0 3], got `Y` instead of `7`"
        "At index 4, got `Z` instead of `8`"])
)

(fact "mixtures"
  (future-fact
    (f '{:a [a Z] :b b :c 1}
       '{:a [a b] :b b     })
    ["At path [:a 1], got `Z` instead of `b`"
     "At key :a, extra key `:c`"])

  (future-fact
    (f '[0 {1 Z} 2 {}]
       '[1 {1 2} 2 {:b 2}])
    ["At index 0, got `0` instead of `1`."
     "At path [1 1], got `Z` instead of `2`."
     "At index 4, missing key `:b`"])

  (future-fact
    (f '[0 {1 [2 Z], 2 3} [Z] [3 {:a Z}]]
       '[0 {1 [2 2], 2 4} [3] [3 {:a 2}]])
    => ["At path [1 1 0], got `Z` instead of `2`"
        "At path [1 2], got `3` instead of `4`"
        "At index [2 0], got `Z` instead of `3`"
        "At path [3 :a], got `Z` instead of `2`"])
)
