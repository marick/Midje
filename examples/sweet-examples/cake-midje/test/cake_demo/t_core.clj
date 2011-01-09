(ns cake-demo.t-core
  (:use clojure.test)
  (:use midje.sweet))

(deftest a-clojure-test-test
  (is (= 1 23)))

(fact 1 => 3)
