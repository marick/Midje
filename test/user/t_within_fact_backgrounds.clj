(ns user.t-within-fact-backgrounds
  (:use midje.sweet
        midje.test-util))

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

(prn "Against-background may be deeply nested")
(future-fact "against-background may be deeply nested"  ;;; Make this more complicated when it actually works.
  (fact
    (against-background (g anything) => 2)
    (f 2) => 2))
    
(fact "against-background can have an optional let-style list"
  (against-background [(g anything) => 2])
  (f 2) => 2)
