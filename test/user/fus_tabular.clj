(ns user.fus-tabular
  (:use midje.sweet
        midje.test-util))

(tabular "Crazy nesting"
  (fact
    (tabular (fact (+ ?a ?b) => ?c)
      ?a ?b
      1  2
      2  1))
  ?c
  3)
