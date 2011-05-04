;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-setup-teardown
  (:use [midje.sweet])
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
 (fact "`around` lexical bindings are available to background prerequisites"
   (against-background (around :facts (let [x 1] ?form))
                       (f x) => 2 )
   (+ (f x) 2) => 4)
 (fact (only-passes? 1) => truthy))

(after-silently 
 (fact "order is unimportant"
   (against-background (f x) => 2
                       (around :facts (let [x 1] ?form)))
   (+ (f x) 2) => 4)
 (fact (only-passes? 1) => truthy))

(after-silently 
 (fact "... and also per-check prerequisites"
   (against-background (around :facts (let [x 1] ?form)))
   (+ (f x) 2) => 4 (provided (f x) => 2 ))
 (fact (only-passes? 1) => truthy))

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

                                        ; ========


(def immediate-atom (atom 0))
(against-background [ (before :contents (swap! immediate-atom (constantly 33))
                              :after (swap! immediate-atom (constantly 110)))
                      (f ...arg...) => 300
                      (around :contents (let [x 1] ?form))
                      (before :facts (swap! per-fact-atom (constantly 18))) ]

  (fact "one set of facts"
    (against-background (before :checks (swap! per-check-atom (constantly 3))
                                :after (swap! per-check-atom (constantly 888))))
      
    @immediate-atom => 33  ;; content wrapper applies
    @per-fact-atom => 18   ;; fact wrapper applies
    @per-check-atom => 3   ;; per-check wrapper applies
    (+ x 33) => 34         ;; content wrapper applies

    (swap! immediate-atom inc) => 34   ;; content wrapper inapplicable.
    (swap! per-fact-atom inc) => 19    ;; fact wrapper inapplicable.
    (swap! per-check-atom inc) => 4    ;; per-check wrapper applies
    (swap! per-check-atom inc) => 4   ;; See?

    (+ (f ...arg...) x) => 301)
  
  (fact "the other"
    @immediate-atom => 34       ;; immediate wrapper doesn't apply.
    @per-fact-atom => 18        ;; per-fact wrapper resets.
    @per-check-atom => 888      ;; old per-check atom applied once, applies no more
    x => 1
    (+ (f ...arg...) x @per-fact-atom) => 319

    (swap! per-fact-atom inc) => 19
    (swap! per-check-atom inc) => 889))

(fact "everything left with final value"
  @immediate-atom => 110        ;; content wrapper set parting value.
  @per-fact-atom => 19
  @per-check-atom => 889

  (swap! per-check-atom inc) => 890
  @per-check-atom => 890)

                                        ; ========
;; How backgrounds that apply to :contents work.

(def per-contents-atom (atom 999))
(background (before :contents (swap! per-contents-atom (constantly 3))))

(fact @per-contents-atom => 3)
(fact
  (swap! per-contents-atom inc) => 4
  @per-contents-atom => 4)
(fact @per-contents-atom => 4)
                                        ; ========

(background (around :facts (let [one 111] ?form)))
(fact  "Background facts are visible even when they don't wrap a fact (old bug)"
  (+ one 222) => 333)

(against-background [(around :facts (let [two 222] ?form))]
  (fact (+ one two) => 333))

                                        ; ========


(against-background [ (around :facts (let [x 1] ?form) )]
  (let [y 2]
    (fact "a let in background plays well with let in regular code"
      (+ x y) => 3)

    (fact "including things like scoping"
      (against-background (around :checks (let [z (* 3 x)
                                                a (* 4 y)] ?form)))
      (+ x y z a) => 14)))

  
