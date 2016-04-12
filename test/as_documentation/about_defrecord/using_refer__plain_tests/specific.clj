;; Using `refer` is perhaps more idiomatic than `:require :as`. A simple record definition
;; need only refer to protocol name. However, self-calls in function definitions require
;; `refer`ences to the generic function. Any use of a generic function in code or tests
;; following the record definition require `refer`ences.

(ns as-documentation.about-defrecord.using-refer--plain-tests.specific
  (:require [midje.sweet :refer :all])
  ;; This is all that's required in simple cases.
  (:require [as-documentation.about-defrecord.generic :refer [Numerical]])
  ;; However, if record definitions need to use other generic functions, or if
  ;; generic functions are to be used after the record definition, they must be
  ;; explicitly `refer`enced/imported.
  (:require [as-documentation.about-defrecord.generic
             :refer [bump ; required because of self-call of bump.
                     twice ; required only so that the test later can refer to it
                     ]]))


(defrecord Record [n]
  Numerical
  (bump [_ by] (+ n by))
  (bump [this] (bump this 1))           ; note self-call
  (twice [this] (bump this (:n this))))

(facts "in creation namespace"
  (let [rec (->Record 3)]
    (bump rec) => 4
    (bump rec 5) => 8
    (twice rec) => 6))
