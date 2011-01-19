;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-laziness
  (:use [midje.sweet]
        [midje.util laziness]
        [midje.test-util]
        [clojure.contrib.seq :only [separate]]))

;; Justification for use of eagerly
(def counter (atom 1))
(def mocked-function-produces-next-element inc)

(defn function-under-test-produces-a-lazy-list []
  (iterate mocked-function-produces-next-element 1))

(defn mock-use []
  (binding [mocked-function-produces-next-element (fn [n] (swap! counter inc) (inc n))]
    (eagerly (take 5 (function-under-test-produces-a-lazy-list)))))

(fact "eagerly forces evaluation"
  (mock-use)
  @counter => 5)

;; After justification, more facts.

(fact "eagerly recursively turns lazy sequences into lists, preserving metadata"
  (let [lazied (with-meta (map identity [1 2 3]) {:hi :mom})]
    (doseq [eagered [ (eagerly lazied)
                      (first (eagerly (map identity [lazied])))
                      (first (eagerly (list lazied)))
                      (first (eagerly [lazied]))
                      (:k (eagerly {:k lazied}))
                      (first (keys (eagerly {lazied 3})))
                      (first (eagerly #{lazied}))
                      ] ]
      (= (type eagered) clojure.lang.LazySeq) => falsey
      eagered => list?
      (meta eagered) => {:hi :mom})))

(fact "eagerly preserves identical? for non-collections."
  (let [eagered (first (eagerly (map identity [odd?])))]
    eagered => #(identical? % odd?)
    (:name (meta eagered)) => #(identical? % (:name (meta odd?)))))

(defrecord Foo [x y])
(fact "preserves record types"
  (class (eagerly (Foo. 4 5))) => Foo
  (Foo. 4 5) => (Foo. 4 5))

(fact "eagerly does NOT preserve identical? for collections even if they had no lazy seqs"
  (let [lazied (with-meta '(1 2 3) {:original :metadata})
        eagered (eagerly lazied)]
    (identical? eagered lazied) => falsey
    (= eagered lazied) => truthy
    (identical? (meta eagered) (meta lazied))))
