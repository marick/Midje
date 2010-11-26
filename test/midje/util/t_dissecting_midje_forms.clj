(ns midje.util.t-dissecting-midje-forms
  (:use [midje.util.dissecting-midje-forms] :reload-all)
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-fact '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])


(fact "separate-fact divides forms into background and other things"
  (separate-fact []) =>
                 [ [] [] ]
  (separate-fact '[ (f 1) => 3 ]) =>
             [ [] '[ (f 1) => 3 ] ]
  (separate-fact '[ (against-background) (f 1) => 3 ]) =>
                                 [ [] '[ (f 1) => 3 ] ]
  (separate-fact '[ (against-background (g) => 22)     (f 1) => 3 ]) =>
                                    [ '[(g) => 22] '[ (f 1) => 3 ] ]
  (separate-fact '[ (against-background (g) => 22)
		    (f 1) => 3
		    (against-background (h) => 3)]) => [ '[(g) => 22 (h) => 3]
							 '[ (f 1) => 3 ] ])
