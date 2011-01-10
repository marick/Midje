(ns leiningen-midje.t-core
  (:use leiningen-midje.core
	clojure.test
	midje.sweet))

(println "The following two failures are intentional.")
(fact
  (f 1) => 1
  (provided
    (g 1) => 1))

(deftest a-failing-test
  (println "The following failure (from deftest) is intentional.")
  (is (= 33 (+ 1 2))))
