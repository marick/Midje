;; There are two kinds of functions now. Helper functions about records are in the `specific`
;; namespace. But the generic functions come from the `generic` namespace.

(ns as-documentation.about-defrecord.using-refer--plain-tests.test
  (:require [midje.sweet :refer :all])
  (:require [as-documentation.about-defrecord.using-refer--plain-tests.specific
             :refer [->Record]])
  (:require [as-documentation.about-defrecord.generic
             :refer [bump twice]]))

(facts "about what exists where"
  (find-var 'as-documentation.about-defrecord.using-refer--plain-tests.specific/->Record) => truthy
  (find-var 'as-documentation.about-defrecord.generic/->Record) => nil
  (find-var 'as-documentation.about-defrecord.using-refer--plain-tests.specific/bump) => nil
  (find-var 'as-documentation.about-defrecord.generic/bump) => truthy)

(facts "you'll use both namespaces to test the record"
  (let [rec (->Record 3)]
    (bump rec) => 4
    (bump rec 5) => 8
    (twice rec) => 6))
