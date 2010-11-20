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

(fact "background prerequisites follow a stack protocol"
  (let [fakes [(fake (unused) => 3 :type :background)
	       (fake (used) => 4 :type :background)]
	more-fakes [ (fake (calls-nothing) => 5 :type :background) ]]

    (expect (background-fakes) => [ '() ]) ;; Facts push their background onto the stack.

    (push-background-fakes fakes)
    (background-fakes) => [ (reverse fakes) '() ]

    (push-background-fakes more-fakes)
    (background-fakes) => [ (reverse more-fakes) (reverse fakes) '() ]

    (pop-background-fakes)
    (background-fakes) => [ (reverse fakes) '() ]

    (pop-background-fakes)
    (background-fakes) => [ '() ]))



(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(fact "background fakes are actually used, but are overridden by more specific fakes"
  (push-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)])
  (calls-used) => "hi mom"
    (provided (local) => "mom")
  (pop-background-fakes))

(fact "one can wrap fakes around `expect` calls"
  (with-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)]
    (calls-used) => "hi mom"
    (provided (local) => "mom"))
  (background-fakes) => [ '() ])


(fact "the background wrapper unwraps even if there are exceptions"
  (with-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)]
    (count (background-fakes)) => (partial = 2)
    (throw (Exception.)) => (throws Exception))
  (background-fakes) => [ '() ])

;; Keeping this because I'm not 100% sure the above test checks the same thing.
(deftest background-wrapper-unwind-protects
  (try
    (with-background-fakes [(fake (unused) => 3 :type :background)
			    (fake (used) => "hi" :type :background)]
      (is (not (empty? (background-fakes))))
      (throw (Exception.)))
    (catch Exception ex (is (empty? (background-fakes))))))

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
		    

(declare middlemost innermost)
(against-background [ (middlemost ...m...) => 33 ]
  (fact "against-background defines metaconstants"
    (against-background (innermost ...i...) => 8)
    (+ (middlemost ...m...) (innermost ...i...)) => 41))


(fact "can wrap one form with another"
  (wrap '(let [r 1] ?form) '(* r 2)) => '(let [r 1] (* r 2)))
