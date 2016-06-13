;; While the *record* exists in the `specific` namespace, the generic functions are only in the
;; `generic` namespace. So we must require them both. That given, tests are unsurprising.

(ns as-documentation.about-defrecord.using-as--provided-tests.test
  (:require [midje.sweet :refer :all]
            [as-documentation.about-defrecord.generic :as generic]
            [as-documentation.about-defrecord.using-as--provided-tests.specific :as specific]))

(fact "testing the record works as in the `using_as__plain_tests` case"
  (let [rec (specific/->Record 3)]
    (generic/bump rec) => 4
    (generic/bump rec 5) => 8
    (generic/twice rec) => 6))

(fact "you can test in terms of a record's methods"
  (let [rec (specific/->Record 3)]
    (+ 1 (generic/bump rec)) => 0
    (provided
      (generic/bump rec) => -1)))

