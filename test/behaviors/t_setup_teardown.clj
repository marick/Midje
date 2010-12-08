(ns behaviors.t-setup-teardown
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

(def test-atom (atom 33))
(after-silently 
 (fact
   (against-background (before :checks (swap! test-atom (constantly 0))))
   (swap! test-atom inc) => 1
   (swap! test-atom dec) => -1)
 (fact (only-passes? 2) => truthy))
    
(def test-atom (atom 0))
(against-background [ (after :checks (swap! test-atom (constantly 0))) ]
  (after-silently 
   (fact
     (swap! test-atom inc) => 1
     (swap! test-atom dec) => -1)
   (fact (only-passes? 2) => truthy)))
    
(def before-atom (atom 10))
(def after-atom (atom 33))
(after-silently 
 (fact
   (against-background (before :checks (swap! before-atom (constantly 0))
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

(against-background [ (around :checks (let [x 1] ?form)) ]
  (after-silently 
   (fact "arbitrary forms can be wrapped around a check"
     (+ x 2) => 3)
   (fact (only-passes? 1) => truthy)))

(after-silently 
 (fact "background wrapping establishes a lexical binding"
   (against-background (around :checks (let [x 1] ?form))
                       (f x) => 2 )
   (+ (f x) 2) => 4)
 (fact (only-passes? 1) => truthy))

(after-silently 
 (fact "prerequisites are scoped within all setup/teardown"
   (against-background (f x) => 2
                       (around :checks (let [x 1] ?form)))
   (+ (f x) 2) => 4)
 (fact (only-passes? 1) => truthy))


;; Separate scoping of prerequisites and setup/teardown
;; doesn't fully work yet. Consider this case:
;(against-background [ (f x) => 2 ]
;  (after 
;   (fact "prerequisites are scoped within all setup/teardown"
;     (against-background (around :checks (let [x 1] ?form)))
;     (+ (f x) 2) => 4)
;   (fact (only-passes? 1) => truthy)))
;; I'm not sure if prerequisite scoping makes sense, so deferring
;; thinking about this.
  
    
;; Here's a possibly more sensible case
(against-background [ (f 1) => 2 ]
  (after-silently 
   (fact "prerequisites are scoped within all setup/teardown"
     (against-background (around :checks (let [x 1] ?form)))
     (+ (f x) 2) => 4)
   (fact (only-passes? 1) => truthy)))

(future-fact "users can make functions that return before/after/around descriptions"
  (against-background (x-is-available-with-value-1))
  x => 1)
    
					; ========


(fact (= @test-atom 18) => falsey)
(against-background [ (before :facts (swap! test-atom (constantly 18))) ]
  (after-silently 
   (fact
     (swap! test-atom inc) => 19
     (swap! test-atom dec) => 18)
   (fact (only-passes? 2) => truthy)))

(def per-fact-atom (atom 0))
(def per-check-atom (atom 0))
(against-background [ (before :facts (swap! per-fact-atom (constantly 18)))
		      (before :checks (swap! per-check-atom (constantly 3))) ]
  (after-silently 
   (fact
     @per-fact-atom => 18
     @per-check-atom => 3
     (+ (swap! per-fact-atom inc) (swap! per-check-atom inc)) => 23
     (+ (swap! per-fact-atom inc) (swap! per-check-atom inc)) => 24
     @per-fact-atom => 20
     @per-check-atom => 3)
   (fact
     (only-passes? 6) => truthy
     @per-fact-atom => 18
     @per-check-atom => 3)))

(fact "around facts work within the fact body"
  (against-background (before :facts (swap! per-fact-atom (constantly 18)))
		      (before :checks (swap! per-check-atom (constantly 3)))
		      (around :facts (let [x 1] ?form)))
  @per-fact-atom => 18
  @per-check-atom => 3
  (+ x 33) => 34

  (swap! per-fact-atom inc)       @per-fact-atom => 19
  (swap! per-check-atom inc)      @per-check-atom => 3) ;; before swap wipes out inc

(against-background [ (around :checks (let [x 1] ?form)) ]
  (facts "about shadowing"
    1 => 1
    x => 1
    (let [x "a shadowing value"] (str "within a let, we see " x)) =>
    "within a let, we see a shadowing value")

  (against-background [ (around :checks (let [x 33 y 12] ?form))
			(around :checks (let [y 10] ?form))]
    (fact "Later values shadow"
      (+ x y) => 43))

  (fact x => 1))

(def immediate-atom (atom 0))
(against-background [ (before :contents (swap! immediate-atom (constantly 33))
			      :after (swap! immediate-atom (constantly 110)))
		      (around :contents (let [x 1] ?form))
		      (before :facts (swap! per-fact-atom (constantly 18))) ]

  (future-fact "one set of facts"
    (against-background (before :checks (swap! per-check-atom (constantly 3))
  				:after (swap! per-check-atom (constantly 888))))
      
    @per-fact-atom => 18
    @per-check-atom => 3
    (+ x 33) => 34

    (swap! @immediate-atom inc) => 34
    (swap! per-fact-atom inc) => 19
    (swap! per-check-atom inc) => 3)

  (future-fact
    @immediate-atom => 34
    @per-fact-atom => 18
    @per-check-atom => 888
    x => 1))

