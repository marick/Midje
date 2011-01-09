(ns cake-demo.t-core
  (:use clojure.test)
  (:use midje.sweet))

(deftest a-clojure-test-test
  (println "\n+++ The following shows how 'cake midje' runs clojure.test deftests.")
  (println "+++ The failure is intentional.")
  (is (= 1 "I am a deftest expected result.")))

(println "\n+++ The following shows how 'cake midje' checks facts in test files.")
(println "+++ The failure is intentional.")
(fact
  (+ 1 2) => "I am a test file fact")
