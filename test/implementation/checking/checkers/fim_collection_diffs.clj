(ns implementation.checking.checkers.fim-collection-diffs (:require [midje.checking.checkers.collection-diffs :as subject])
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.checking.core :as core])
  (:import [flare.map MapKeysDiff]
           [flare.atom AtomDiff]))

(defrecord R [])

(fact classifications
  (subject/classify {} {} #{}) => :map:map
  (subject/classify {} {} #{:gaps-ok}) => :map:map
  (subject/classify {} {} #{:in-any-order}) => :map:map

  (subject/classify {} (R.) #{}) => :map:map
  (subject/classify (R.) {} #{:gaps-ok :in-any-order}) => :map:map
  (subject/classify (R.) (R.) #{}) => :map:map

  (subject/classify :other :things #{}) => :unknown)





;; Eventually, the `differ` will be the actual checker, making this format a bit more
;; readable.
(defn result-of
  ([actual differ expected looseness] (differ actual expected looseness))
  ([actual differ expected] (result-of actual differ expected #{})))

(defrecord TwoArgs [l1 l2])

(fact "maplike things"
  (fact "success cases"
    (result-of {:l1 1} subject/contains:diffs {:l1 1}) => empty?
    (result-of {:l1 1} subject/contains:diffs {}) => empty?
    (result-of {:l1 1 :l2 1} subject/contains:diffs {:l1 1}) => empty?)

  (fact "missing keys"
    (result-of {} subject/contains:diffs {:l1 1}) => [(new MapKeysDiff #{:l1} nil)]
    (result-of {:l1 1} subject/contains:diffs {:l1 1 :l2 2 :l3 3}) => [(new MapKeysDiff #{:l2 :l3} nil)])

  (fact "incorrect values"
    (result-of {:l1 1} subject/contains:diffs {:l1 2}) => [{:l1 (new AtomDiff 1 2)}]
    (result-of {:l1 1, :l2 :irrelevant} subject/contains:diffs {:l1 2}) => [{:l1 (new AtomDiff 1 2)}]

    ;; does not try to compare a missing key
    (result-of {:l1 1, :l2 2} subject/contains:diffs {:l1 11, :l2 22 :l3 3})
    => (just (new MapKeysDiff #{:l3} nil)
             {:l1 (new AtomDiff 1 11) :l2 (new AtomDiff 2 22)}
             :in-any-order))

  (fact "Records are processed as maps"
    (result-of (new TwoArgs 1 2) subject/contains:diffs {:l1 1}) => empty?
    (result-of (new TwoArgs 1 2) subject/contains:diffs {:l1 2}) => [{:l1 (new AtomDiff 1 2)}]
    (result-of (new TwoArgs 1 2) subject/contains:diffs {:l1 2 :l2 2 :l3 3})
    => (just {:l1 (new AtomDiff 1 2)}
             (new MapKeysDiff #{:l3} nil)
             :in-any-order)

    (result-of (new TwoArgs 1 2) subject/contains:diffs (new TwoArgs 1 2)) => empty?
    (result-of (new TwoArgs 1 2) subject/contains:diffs (new TwoArgs 2 2)) => [{:l1 (new AtomDiff 1 2)}])

  (future-fact "compares values using extended equality"
    (let [result (result-of {:l1 "12"} subject/contains:diffs {:l1 #"\d\d\d"})
          atom-diff (:l1 (first result))]
      (:a atom-diff) => "12"
      (str (:b atom-diff)) => (str #"\d\d\d"))

    (result-of {:l1 1} subject/contains:diffs {:l1 odd?}) => empty?

    (prn "=============")
    (result-of {:l1 2} subject/contains:diffs {:l1 odd?}) => [{:l1 (new AtomDiff 2 'odd?)}]
    )

  (future-fact "Note that extended-equality matches aren't erroneously included in the output")

  (future-fact "embedded checkers 'bubble up'")
)
