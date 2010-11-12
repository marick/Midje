(ns midje.semi-sweet.t-semi-sweet-internals
  (:use clojure.test)
  (:use [midje.semi-sweet.semi-sweet-internals] :reload-all)
  (:use [midje.test-util]))

(deftest separation-test
  (let [actual (separate '( (fake (f 1) => 2) :key 'value))]
    (is (= '[:key 'value] (first actual)))
    (is (= '[(fake (f 1) => 2)] (second actual))))

  (let [actual (separate '( (not-called some-function) :key 'value))]
    (is (= '[:key 'value] (first actual)))
    (is (= '[(not-called some-function)] (second actual))))

  ;; often passed a seq.
  (let [actual (separate (seq '( (fake (f 1) => 2) :key 'value)))]
    (is (= '[:key 'value] (first actual)))
    (is (= '[(fake (f 1) => 2)] (second actual))))

  ;; Either arg may be omitted.
  (let [actual (separate '())]
    (is (= [] (first actual)))
    (is (= [] (second actual))))
)  

(deftest midje-override-map-test
  (is (= {} (midje-override-map [])))
  (is (= {:a 1 :b 2} (midje-override-map [:a 1 :b 2])))
  (is (= {:a 1 :b 33333} (midje-override-map [:a 1 :b 2 :b 33333])))
)

