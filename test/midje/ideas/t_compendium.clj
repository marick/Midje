(ns midje.ideas.t-compendium
  (:use [midje.sweet]
        [midje.test-util]
        [midje.ideas.compendium]))

;;; Recently-run facts.

(def run-count (atom 0))
(fact
  (swap! run-count inc)
  (+ 1 1) => 2)
(recheck-fact)
(fact @run-count => 2)
(let [definition (source-of-last-fact-checked)]
  (fact definition => '(fact @run-count => 2)))

(def outer-run-count (atom 0))
(def inner-run-count (atom 0))
(fact "The last fact check is the outermost nested check"
  (swap! outer-run-count inc)
  (+ 1 1) => 2
  (fact "inner fact"
    (swap! inner-run-count inc)
    (fact (- 1 1) => 0)))
(recheck-fact)

(let [fact-name (:midje/name (meta last-fact-checked))]
  (fact
    "The last fact check is the outermost nested check"
    @outer-run-count => 2
    @inner-run-count => 2))


(def run-count (atom 0))
(fact "outermost"
  (fact "inner 1"
    (swap! run-count inc))
  (fact "inner 2"
    (swap! run-count inc)))
(recheck-fact)
(fact
  @run-count => 4)
  
(def run-count (atom 0))
(tabular "tabular facts count as last-fact checked"
  (fact
    (swap! run-count inc)
    (+ ?a ?b) => ?c)
  ?a ?b ?c
  1  2  3
  2  2  4)
(recheck-fact)
(fact @run-count => 4)

;; Facts mark themselves as last-fact-checked each time they're rechecked.

(fact (+ 1 1) => 2)
(def one-plus-one (last-fact-checked))
(fact (+ 2 2) => 4)
(def two-plus-two (last-fact-checked))

(recheck-fact)
(let [previous (last-fact-checked)]
  (fact previous => (exactly two-plus-two)))

(one-plus-one)
(let [previous (last-fact-checked)]
  (fact previous => (exactly one-plus-one)))
