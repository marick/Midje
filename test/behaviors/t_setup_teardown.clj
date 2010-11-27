(ns behaviors.t-setup-teardown
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

(def test-atom (atom 0))

(future-fact
  (against-background (before :checking (swap! test-atom (constantly 0))))
  (swap! test-atom inc)
  (fact @test-atom => 1)
  (swap! test-atom dec)
  (fact @test-atom => -1))
    
    
    
