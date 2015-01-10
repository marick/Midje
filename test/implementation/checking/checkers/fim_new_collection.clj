(ns implementation.checking.checkers.fim-new-collection (:require [midje.checking.checkers.new-collection :as subject])
  (:use midje.sweet midje.test-util)
  (:require [midje.checking.core :as core])
  (:import [flare.map MapKeysDiff]))
            

(defn c [actual checker expected]
  (let [result ((checker expected) actual)]
    (if (core/data-laden-falsehood? result)
      (:diffs (core/data-laden-falsehood-to-map result))
      result)))
  
(fact "success cases"
  (c {:a 1} subject/contains {:a 1}) => truthy
  (c {:a 1} subject/contains {}) => truthy
  (c {:a 1 :b 1} subject/contains {:a 1}) => truthy)


(fact "missing keys"
  (c {} subject/contains {:a 1}) => [(new MapKeysDiff #{:a} nil)]
  (c {:a 1} subject/contains {:a 1 :b 2 :c 3}) => [(new MapKeysDiff #{:b :c} nil)])


(future-fact "compares values using extended equality"
  (c {:a "123"} subject/contains {:a #"\d\d\d"}) => 1 ; [(new ExpectedRegexpDiff #"\d\d\d")]
  (c {:a 1} subject/contains {:a odd?}) => 1; [(new MapKeysDiff #{:a})])
)




