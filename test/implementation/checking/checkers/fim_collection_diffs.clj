(ns implementation.checking.checkers.fim-collection-diffs (:require [midje.checking.checkers.collection-diffs :as subject])
  (:use midje.sweet midje.test-util)
  (:require [midje.checking.core :as core])
  (:import [flare.map MapKeysDiff]))
            

;; Eventually, the `differ` will be the actual checker, making this format a bit more
;; readable.
(defn c [actual differ expected]
  (differ actual expected))
  ;; (let [result ((checker expected) actual)]
  ;;   (if (core/data-laden-falsehood? result)
  ;;     (:diffs (core/data-laden-falsehood-to-map result))
  ;;     result)))
  
(fact "success cases"
  (c {:a 1} subject/contains:diffs {:a 1}) => empty?
  (c {:a 1} subject/contains:diffs {}) => empty?
  (c {:a 1 :b 1} subject/contains:diffs {:a 1}) => empty?)


(fact "missing keys"
  (c {} subject/contains:diffs {:a 1}) => [(new MapKeysDiff #{:a} nil)]
  (c {:a 1} subject/contains:diffs {:a 1 :b 2 :c 3}) => [(new MapKeysDiff #{:b :c} nil)])

(future-fact "compares values using extended equality"
  (c {:a "123"} subject/contains:diffs {:a #"\d\d\d"}) => 1 ; [(new ExpectedRegexpDiff #"\d\d\d")]
  (c {:a 1} subject/contains:diffs {:a odd?}) => 1; [(new MapKeysDiff #{:a})])
)




