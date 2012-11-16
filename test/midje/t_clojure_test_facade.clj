(ns midje.t-clojure-test-facade
  (:use midje.sweet
        midje.clojure-test-facade
        midje.test-util)
  (:require clojure.test))

;; Can run with isolated counters

;; Ensure there's at least three successes queued up.
(fact 1 => 1)
(fact 2 => 2)
(fact 3 => 3)
(def original (counters))

(with-isolated-counters
  (fact (counters)) => (contains {:pass 0, :fail 0})
  (fact (counters)) => (contains {:pass 1, :fail 0}))
(fact (counters) => original)
    
;; Can clear counters

(with-isolated-counters
  (fact (counters)) => (contains {:pass 0, :fail 0})
  (zero-counters)
  (fact (counters)) => (contains {:pass 0, :fail 0}))

;;; run-tests

(let [result (with-isolated-counters
               (run-tests ['midje.t-clojure-test-facade]))]
  (fact
    (:test-count result) => 0
    (:fail-count result) => 0
    (:lines result) => ["",
                        "Ran 0 tests containing 0 assertions."
                        "0 failures, 0 errors."]))

(clojure.test/deftest a-clojure-test-pass
  (clojure.test/is (= 1 1)))

(let [result (with-isolated-counters
               (run-tests ['midje.t-clojure-test-facade]))]
  (fact
    (:test-count result) => 1
    (:fail-count result) => 0
    (:lines result) => ["",
                        "Ran 1 tests containing 1 assertions."
                        "0 failures, 0 errors."]))

(clojure.test/deftest a-clojure-test-fail
  (clojure.test/is (= 1 2)))

(let [result (with-isolated-counters
               (run-tests ['midje.t-clojure-test-facade]))]
  (fact
    (:test-count result) => 2
    (:fail-count result) => 1
    (nth (:lines result) 1) => #"FAIL in.*a-clojure-test-fail"
    (nth (:lines result) 2) => #"expected"
    (nth (:lines result) 3) => #"actual"
    (take-last 2 (:lines result)) => ["Ran 2 tests containing 2 assertions."
                                      "1 failures, 0 errors."]))

(ns-unmap *ns* 'a-clojure-test-fail) ; so as not to see failure when test rerun.
