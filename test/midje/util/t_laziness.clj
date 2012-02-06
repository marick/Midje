;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-laziness
  (:use [midje.sweet]
        [midje.util laziness thread-safe-var-nesting]
        [midje.test-util]))

;; Justification for use of eagerly
(def counter (atom 1))
(def #^:dynamic *mocked-function-produces-next-element* inc)

(defn function-under-test-produces-a-lazy-list []
  (iterate *mocked-function-produces-next-element* 1))

(defn mock-use []
  (binding [*mocked-function-produces-next-element* (fn [n] (swap! counter inc) (inc n))]
    (eagerly (take 5 (function-under-test-produces-a-lazy-list)))))

(fact "eagerly forces evaluation"
  (mock-use)
  @counter => 5)

;; After justification, more facts.

(unfinished exploder)
(map exploder [1 2 3])

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

(fact "absence"
  (with-altered-roots {#'exploder identity} #{ (map #'exploder [1 2 3]) })
  => #{[1 2 3]}
  (first (with-altered-roots {#'exploder identity} [#{ (map #'exploder [1 2 3]) }]))
  => #{[1 2 3]}
  (first (with-altered-roots {#'exploder identity} (list #{  (map #'exploder [1 2 3]) })))
  => #{[1 2 3]}
  (:k (with-altered-roots {#'exploder identity} {:k #{ (map #'exploder [1 2 3]) }}))
  => #{[1 2 3]}
  (first (keys (with-altered-roots {#'exploder identity} { #{ (map #'exploder [1 2 3]) } 'foo})))
  => #{[1 2 3]}
  (:x (with-altered-roots {#'exploder identity} (Foo. #{ (map #'exploder [1 2 3]) } 'x)))
  => #{[1 2 3]}
  )

(fact "about how eagerly improves things 2"
  (with-altered-roots {#'exploder identity} (eagerly #{ (map #'exploder [1 2 3]) }))
  => #{[1 2 3]}
  (first (with-altered-roots {#'exploder identity} (eagerly [#{ (map #'exploder [1 2 3]) }])))
  => #{[1 2 3]}
  (first (with-altered-roots {#'exploder identity} (eagerly (list #{  (map #'exploder [1 2 3]) }))))
  => #{[1 2 3]}
  (:k (with-altered-roots {#'exploder identity} (eagerly {:k #{ (map #'exploder [1 2 3]) }})))
  => #{[1 2 3]}
  (first (keys (with-altered-roots {#'exploder identity} (eagerly { #{ (map #'exploder [1 2 3]) } 'foo} ))))
  => #{[1 2 3]}
  (:x (with-altered-roots {#'exploder identity} (eagerly (Foo. #{ (map #'exploder [1 2 3]) } 'x))))
  => #{[1 2 3]}
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

(fact "eagerly does NOT preserve identical? for collections even if they had no lazy seqs"
  (let [lazied (with-meta '(1 2 3) {:original :metadata})
        eagered (eagerly lazied)]
    (identical? eagered lazied) => falsey
    (= eagered lazied) => truthy
    (identical? (meta eagered) (meta lazied))))


(deftype MySet []
  clojure.lang.IPersistentSet
  (disjoin [this k]
    (let [now (System/nanoTime)]
      ))

  (get [this k]
    k)

  (seq [this]
    (seq []))

  (count [this]
    (count (seq this)))

  clojure.lang.IPersistentCollection
  (cons [this k]
    (let [now (System/nanoTime)]
      ))

  (empty [this]
    [])

  (equiv [this other]
    (.equals this other))

  Object
  (hashCode [this]
    1)

  (equals [this other]
    (or (identical? this other)
      (and (instance? java.util.Set other)
        (let [^java.util.Set o (cast java.util.Set other)]
          (and (= (count this) (count o))
            (every? #(contains? o %) (seq this)))))))

  (toString [this]
    (throws Exception "boom"))

  java.util.Set
  (contains [this k]
    (throws Exception "boom"))

  (containsAll [this ks]
    (throws Exception "boom"))

  (size [this]
    (throws Exception "boom"))

  (isEmpty [this]
    (throws Exception "boom"))

  (toArray [this]
    (throws Exception "boom"))

  (toArray [this dest]
    (throws Exception "boom")
    ))

(fact 
  (MySet.) => (MySet.))