(ns midje.util.t-laziness
  (:require [midje.sweet :refer :all]
            [midje.util
             [laziness :refer :all]
             [thread-safe-var-nesting :refer :all]]
            [midje.test-util :refer :all]))

;; Justification for use of eagerly
(def counter (atom :needs-to-be-initialized))
(def #^:dynamic *mocked-function-produces-next-element* inc)

(defn function-under-test-produces-a-lazy-list []
  (iterate *mocked-function-produces-next-element* 1))

(defn mock-use []
  (binding [*mocked-function-produces-next-element* (fn [n] (swap! counter inc) (inc n))]
    (eagerly (take 5 (function-under-test-produces-a-lazy-list)))))

(fact "eagerly forces evaluation"
  (reset! counter 1)
  (mock-use)
  @counter => 5)

;; After justification, more facts.

(unfinished exploder)

;; The following is lazy, so it should not cause an error. In 1.7-alpha5, it does.
;(map exploder [1 2 3])

(defrecord Foo [x y])

(facts "about what happens in the absence of eagerly"
  (with-altered-roots {#'exploder identity} (map #'exploder [1 2 3]))
  => (throws Error)
  (first (with-altered-roots {#'exploder identity} [(map #'exploder [1 2 3])]))
  => (throws Error)
  (first (with-altered-roots {#'exploder identity} (list (map #'exploder [1 2 3]))))
  => (throws Error)
  (:k (with-altered-roots {#'exploder identity} {:k (map #'exploder [1 2 3])}))
  => (throws Error)
  (first (keys (with-altered-roots {#'exploder identity} {(map #'exploder [1 2 3]) 'foo})))
  => (throws Error)
  (:x (with-altered-roots {#'exploder identity} (Foo. (map #'exploder [1 2 3]) 'x)))
  => (throws Error)
)


(fact "about how eagerly improves things"
  (with-altered-roots {#'exploder identity} (eagerly (map #'exploder [1 2 3])))
  => [1 2 3]
  (first (with-altered-roots {#'exploder identity} (eagerly [(map #'exploder [1 2 3])])))
  => [1 2 3]
  (first (with-altered-roots {#'exploder identity} (eagerly (list (map #'exploder [1 2 3])))))
  => [1 2 3]
  (:k (with-altered-roots {#'exploder identity} (eagerly {:k (map #'exploder [1 2 3])})))
  => [1 2 3]
  (first (keys (with-altered-roots {#'exploder identity} (eagerly {(map #'exploder [1 2 3]) 'foo}))))
  => [1 2 3]
  (first (with-altered-roots {#'exploder identity} (eagerly #{(map #'exploder [1 2 3])})))
  => [1 2 3]
  (:x (with-altered-roots {#'exploder identity} (eagerly (Foo. (map #'exploder [1 2 3]) 'x))))
  => [1 2 3]
  )

(fact "eagerly preserves metadata"
  (meta (eagerly (with-meta (map identity [1 2 3]) {:hi :mom})))
  => {:hi :mom})

(fact "eagerly preserves identical? for non-collections."
  (let [eagered (first (eagerly (map identity [odd?])))]
    eagered => #(identical? % odd?)
    (:name (meta eagered)) => #(identical? % (:name (meta odd?)))))

(fact "eagerly preserves record types"
  (class (eagerly (Foo. 4 5))) => Foo
  (eagerly (Foo. 4 5)) => (Foo. 4 5)
  (eagerly [(Foo. 4 5)]) => [(Foo. 4 5)])

(defn- nearby-item-comparator [a b]
  (let [item-key (juxt :timestamp :id)]
    (compare (item-key b) (item-key a))))

(fact "eagerly doesn't barf on sorted-sets with custom comparators... See issue: #158"
      (eagerly (sorted-set-by nearby-item-comparator {:timestamp 0 :id 0} {:timestamp 1 :id 1}))
      => (sorted-set-by nearby-item-comparator {:timestamp 0 :id 0} {:timestamp 1 :id 1}))

(fact "eagerly does NOT preserve identical? for collections even if they had no lazy seqs"
  (let [lazied (with-meta '(1 2 3) {:original :metadata})
        eagered (eagerly lazied)]
    (identical? eagered lazied) => falsey
    (= eagered lazied) => truthy
    (identical? (meta eagered) (meta lazied))))
