(ns midje.emission.t-clojure-test-facade
  (:use midje.sweet
        midje.emission.clojure-test-facade
        midje.test-util)
  (:require clojure.test))

;;; run-tests

(let [result (run-tests ['midje.emission.t-clojure-test-facade])]
  (fact
    :check-only-at-load-time
    (:test result) => 0
    (:fail result) => 0
    (:lines result) => ["",
                        "Ran 0 tests containing 0 assertions."
                        "0 failures, 0 errors."]))

(clojure.test/deftest a-clojure-test-pass
  (clojure.test/is (= 1 1)))

(let [result (run-tests ['midje.emission.t-clojure-test-facade])]
  (fact
    (:test result) => 1
    (:fail result) => 0
    (:lines result) => ["",
                        "Ran 1 tests containing 1 assertions."
                        "0 failures, 0 errors."]))

(clojure.test/deftest a-clojure-test-fail
  (clojure.test/is (= 1 2)))

(let [result (run-tests ['midje.emission.t-clojure-test-facade])]
  (fact
    (:test result) => 2
    (:fail result) => 1
    (nth (:lines result) 1) => #"FAIL in.*a-clojure-test-fail"
    (nth (:lines result) 2) => #"expected"
    (nth (:lines result) 3) => #"actual"
    (take-last 2 (:lines result)) => ["Ran 2 tests containing 2 assertions."
                                      "1 failures, 0 errors."]))

(ns-unmap *ns* 'a-clojure-test-fail) ; so as not to see failure when test rerun.
