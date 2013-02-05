(ns user.t-within-fact-backgrounds
  (:use midje.sweet
        midje.test-util)
  (:require [midje.data.fact :as fact-data]
            [midje.data.compendium :as compendium]))

(unfinished g)

(defn f [n] (g n))

(fact "against-background works within a fact"
  (against-background (g anything) => 2)
  (f 2) => 2)

(fact "against-background can also be placed at the end"
  (f 2) => 2
  (against-background (g anything) => 2))

(fact "background is an acceptable alias"
  (background (g anything) => 2)
  (f 2) => 2)

(fact "against-background can have an optional let-style list"
  (against-background [(g anything) => 2])
  (f 2) => 2)

;;; in-fact against-background does not affect creation of source metadata
  
(fact
  (against-background (g anything) => 2)
  (f 2) => 2)
(fact :check-only-at-load-time
  (fact-data/source (compendium/last-fact-checked<>)) => '(fact (against-background (g anything) => 2) (f 2) => 2)
  (fact-data/guid (compendium/last-fact-checked<>)) => '((against-background (g anything) => 2) (f 2) => 2))

;;; Just for the heck of it, also check for surrounding against-background.

(against-background [(g anything) => 2]
  (fact (f 2) => 2))
(fact :check-only-at-load-time
  (fact-data/source (compendium/last-fact-checked<>)) => '(fact (f 2) => 2)
  (fact-data/guid (compendium/last-fact-checked<>)) => '((f 2) => 2))


