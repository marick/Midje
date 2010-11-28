(ns behaviors.t-setup-teardown
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

(def test-atom (atom 0))
(after 
 (fact
   (against-background (before :checking (swap! test-atom (constantly 0))))
   (swap! test-atom inc) => 1
   (swap! test-atom dec) => -1)
 (fact (only-passes? 2) => truthy))
    
(def test-atom (atom 0))
(against-background [ (after :checking (swap! test-atom (constantly 0))) ]
  (after 
   (fact
     (swap! test-atom inc) => 1
     (swap! test-atom dec) => -1)
   (fact (only-passes? 2) => truthy)))
    
(def before-atom (atom 10))
(def after-atom (atom 33))
(after 
 (fact
   (against-background (before :checking (swap! before-atom (constantly 0))
			       :after    (swap! after-atom (constantly 10))))
   ;; [10 33]
   [(swap! before-atom inc) (swap! after-atom inc)] => [1 34]
   ;; [1 10]
   [(swap! before-atom inc) (swap! after-atom inc)] => [1 11]
   ;; [1 10]
  
   (let [untouched [@before-atom @after-atom]]
     untouched => [1 10]))
 (fact (only-passes? 3) => truthy))

(unfinished f)

(future-fact
 (against-background (around :checking (let [x 1] ?form)
			     (f x) => 2)
  (+ (f x) 2) => 4))
  
  
    
