(ns lein-test.test.with-test
  (:use [lein-test.core] :reload)
  (:use [clojure.test]
	[midje.sweet]))

(with-test
  (defn foo [x y] (* x y))
  (fact (foo 1 2) => 3))
