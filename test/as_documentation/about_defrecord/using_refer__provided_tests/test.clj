;; There are two kinds of functions now. Helper functions about records are in the `specific`
;; namespace. But the generic functions come from the `generic` namespace.

(ns as-documentation.about-defrecord.using-refer--provided-tests.test
  (:use midje.sweet)
  (:require [as-documentation.about-defrecord.generic
             :refer [bump twice]])
  (:require [as-documentation.about-defrecord.using-refer--provided-tests.specific
             :refer [->Record]]))

(facts "in separate test namespace"
  (let [rec (->Record 3)]
    (bump rec) => 4
    (bump rec 5) => 8
    (twice rec) => 6))

(fact "you can test in terms of a record's methods"
  (let [rec (->Record 3)]
    (+ 1 (bump rec)) => 0
    (provided
      (bump rec) => -1)))

