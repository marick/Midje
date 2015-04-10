;; While the *record* exists in the `specific` namespace, the generic functions are only in the
;; `generic` namespace. So we must require them both.

(ns as-documentation.about-defrecord.using-as--plain-tests.test
  (:use midje.sweet)
  (:require [as-documentation.about-defrecord.generic :as generic]
            [as-documentation.about-defrecord.using-as--plain-tests.specific :as specific]))


(facts "about what exists where"
  (find-var 'as-documentation.about-defrecord.using-as--plain-tests.specific/->Record) => truthy
  (find-var 'as-documentation.about-defrecord.using-as--plain-tests.specific/bump) => nil
  (find-var 'as-documentation.about-defrecord.generic/bump) => truthy)

(fact "And so here we see how the two sets of functions are used"
  (let [rec (specific/->Record 3)]
    (generic/bump rec) => 4
    (generic/bump rec 5) => 8
    (generic/twice rec) => 6))
