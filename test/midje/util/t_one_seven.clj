(ns midje.util.t-one-seven
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

(fact "1.7alpha bug: eagerly forces evaluation"
  (reset! counter 1)
  (mock-use)
  @counter => 5
)

;; (midje.checking.facts/creation-time-check
;;  (midje.data.compendium/record-fact-existence!
;;   (clojure.core/with-meta
;;    (clojure.core/fn
;;     []
;;     (midje.parsing.util.wrapping/midje-wrapped
;;      (midje.data.prerequisite-state/with-installed-fakes
;;       (midje.parsing.1-to-explicit-form.parse-background/background-fakes)
;;       (reset! counter 1)
;;       (mock-use)
;;       (midje.parsing.util.wrapping/midje-wrapped
;;        (midje.checking.checkables/check-one
;;         (clojure.core/merge
;;          {:description (midje.data.nested-facts/descriptions),
;;           :expected-result-form '5,
;;           :check-expectation :expect-match,
;;           :midje.parsing.lexical-maps/a-midje-checkable-map? true,
;;           :function-under-test (clojure.core/fn [] @counter),
;;           :expected-result 5,
;;           :position
;;           (pointer.core/line-number-known nil),
;;           :namespace clojure.core/*ns*}
;;          {:arrow '=>, :call-form '@counter}
;;          (commons.clojure.core/hash-map-duplicates-ok
;;           :position
;;           (pointer.core/line-number-known 18)))
;;         [])))))
;;    (clojure.core/merge
;;     {:midje/guid "9a01710b2962f9945f24630a0579bba82671d1ef",
;;      :midje/name "eagerly forces evaluation",
;;      :midje/description "eagerly forces evaluation",
;;      :midje/source
;;      '(fact
;;        "eagerly forces evaluation"
;;        (reset! counter 1)
;;        (mock-use)
;;        @counter
;;        =>
;;        5),
;;      :midje/namespace 'midje.util.t-one-seven,
;;      :midje/file "midje/util/t_one_seven.clj",
;;      :midje/line 17}
;;     {:midje/top-level-fact? true}))))

;; After justification, more facts.

(unfinished exploder)

;; The following is lazy, so it should not cause an error.
(map exploder [1 2 3])

(defrecord Foo [x y])
