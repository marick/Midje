(ns midje.t-background
  (:use clojure.test)
  (:use [midje.background])
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint))


(unfinished unused used)
(defn calls-nothing [] )

(fact "background prerequisites don't have to be used"
  (expect (calls-nothing) => nil
	  (fake (unused) => 3 :type :background)))

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
