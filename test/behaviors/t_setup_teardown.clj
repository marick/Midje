(ns behaviors.t-setup-teardown
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

(def test-atom (atom 0))
(def test-atom2 (atom 0))

(defn f [] 1)

(fact
  (against-background (before :checking (swap! test-atom (constantly 0))))
  (swap! test-atom inc) => 1
  (swap! test-atom dec) => -1
  )

    
(unfinished f)


(future-fact
 (against-background (around :checking (let [x 1] ?form)
			     (f x) => 2)
  (+ (f x) 2) => 4))
  
  
    
