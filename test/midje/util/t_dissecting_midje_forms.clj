(ns midje.util.t-dissecting-midje-forms
  (:use [midje.util.dissecting-midje-forms] :reload-all)
  (:use midje.sweet)
  (:use midje.test-util)
)

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-background-forms '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])


(fact "separate-background-forms divides forms into background and other things"
  (separate-background-forms []) =>
                 [ [] [] ]
  (separate-background-forms '[ (f 1) => 3 ]) =>
             [ [] '[ (f 1) => 3 ] ]
  (separate-background-forms '[ (against-background) (f 1) => 3 ]) =>
                                 [ [] '[ (f 1) => 3 ] ]
  (separate-background-forms '[ (against-background (g) => 22)     (f 1) => 3 ]) =>
                                    [ '[(g) => 22] '[ (f 1) => 3 ] ]
  (separate-background-forms '[ (against-background (g) => 22)
		    (f 1) => 3
		    (against-background (h) => 3)]) => [ '[(g) => 22 (h) => 3]
							 '[ (f 1) => 3 ] ])
