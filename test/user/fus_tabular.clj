(ns user.fus-tabular
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;;; Some pathological nesting cases


(tabular "tabular inside tabular"
  (fact
    (tabular (fact (+ ?a ?b) => ?c)
      ?a ?b
      1  2
      2  1))
  ?c
  3)

(silent-tabular "the error output is reasonable"
  (fact
    (tabular (fact (+ ?a ?b) => ?c)
      ?a ?b
      0  4
      2  3))
  ?c
  4)
(note-that (fact-failed-with-table-bindings '{?a 2, ?b 3, ?c 4}))

(silent-tabular "fact inside fact inside tabular"
  (fact ?a => 2
    (fact 3 => ?a))
  ?a
  2)
(note-that (fact-failed-with-table-bindings '{?a 2}))


(declare g)
(defn f [n] (g n))

(silent-tabular "Facts with prerequisites"
  (fact
    (prerequisite (g ?a) => 2)
    (f ?a) => 3)
  ?a
  2)
(note-that (fact-failed-with-table-bindings '{?a 2}))


;;; [[INTERACTION CASE]]
;;; Because the tabular form expands into a top-level fact (so that it's a
;;; recheckable unit), fact-wide state changes apply to it as well as to all
;;; enclosed facts. That's potentially annoying.

(unfinished g)

(defn f [arg] (g arg))

(def before-fact (atom 0))
(def before-check (atom 0))


(with-state-changes [(before :facts (swap! before-fact inc))
                     (before :checks (swap! before-check inc))]
  (tabular
    (fact
      (prerequisite (g "OK3") => 3)
      (f ?foo) => 3
      (f ?foo) => 3)
    ?foo
    "OK3"
    "OK3"))

(fact
  @before-fact => 3    ; surprisingly not 2
  @before-check => 4)

;; The workaround is to put the state changes within the fact:

(reset! before-fact 0)
(reset! before-check 0)

(tabular
  (fact
    (with-state-changes [(before :facts (swap! before-fact inc))
                         (before :checks (swap! before-check inc))])
    (prerequisite (g "OK3") => 3)
    (f ?foo) => 3
    (f ?foo) => 3)
  ?foo
  "OK3"
  "OK3")

(fact
  @before-fact => 2
  @before-check => 4)
