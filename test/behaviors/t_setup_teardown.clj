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

(against-background [ (around :checking (let [x 1] ?form)) ]
  (after 
   (fact "arbitrary forms can be wrapped around a check"
     (+ x 2) => 3)
   (fact (only-passes? 1) => truthy)))

(after 
 (fact "background wrapping establishes a lexical binding"
   (against-background (around :checking (let [x 1] ?form))
                       (f x) => 2 )
   (+ (f x) 2) => 4)
 (fact (only-passes? 1) => truthy))

(after 
 (fact "prerequisites are scoped within all setup/teardown"
   (against-background (f x) => 2
                       (around :checking (let [x 1] ?form)))
   (+ (f x) 2) => 4)
 (fact (only-passes? 1) => truthy))


;; Separate scoping of prerequisites and setup/teardown
;; doesn't fully work yet. Consider this case:
;(against-background [ (f x) => 2 ]
;  (after 
;   (fact "prerequisites are scoped within all setup/teardown"
;     (against-background (around :checking (let [x 1] ?form)))
;     (+ (f x) 2) => 4)
;   (fact (only-passes? 1) => truthy)))
;; I'm not sure if prerequisite scoping makes sense, so deferring
;; thinking about this.
  
    
;; Here's a possibly more sensible case
(against-background [ (f 1) => 2 ]
  (after 
   (fact "prerequisites are scoped within all setup/teardown"
     (against-background (around :checking (let [x 1] ?form)))
     (+ (f x) 2) => 4)
   (fact (only-passes? 1) => truthy)))
  
    
