(ns midje.t-background
  (:use clojure.test)
  (:use [midje.background])
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint))


(unfinished unused used)
(defn calls-nothing [] )

(deftest background-fakes-need-not-be-called
  (expect (calls-nothing) => nil
	  (fake (unused) => 3 :type :background)))

(deftest pushing-and-popping
  (is (empty? (background-fakes)))
  (let [fakes [(fake (unused) => 3) (fake (used) => 4)]
	more-fakes [ (fake (calls-nothing) => 5) ]]
    (push-background-fakes fakes)
    (is (= [ (reverse fakes) ] (background-fakes)))
    (push-background-fakes more-fakes)
    (is (= [(reverse more-fakes) (reverse fakes)] (background-fakes)))
    (pop-background-fakes)
    (is (= [(reverse fakes)] (background-fakes)))
    (pop-background-fakes)
    (is (empty? (background-fakes)))))



(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(deftest implicit-use-of-background-fakes
  (push-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)])
  (expect (calls-used) => "hi mom"
	  (fake (local) => "mom"))
  (pop-background-fakes))

(deftest background-wrapper
  (with-background-fakes [(fake (unused) => 3 :type :background)
			  (fake (used) => "hi" :type :background)]
    (expect (calls-used) => "hi mom"
	    (fake (local) => "mom")))
  (is (empty? (background-fakes))))

(deftest background-wrapper-unwind-protects
  (try
    (with-background-fakes [(fake (unused) => 3 :type :background)
			    (fake (used) => "hi" :type :background)]
      (is (not (empty? (background-fakes))))
      (throw (Exception.)))
    (catch Exception ex (is (empty? (background-fakes))))))



(deftest expanding-background-forms
  (is (= []  (expand [])))
  (is (= '[(midje.semi-sweet/fake (f 1) => 2 :type :background)]
	 (expand '[(f 1) => 2])))
  (is (= '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar :type :background)
	   (midje.semi-sweet/fake (f 2) => 33 :type :background) ]
	 (expand '[   (f 1) => 2 :foo 'bar (f 2) => 33 ]))))


(deftest separating-background-from-facts-test
  (is (= [ [] [] ] (separate-fact [])))
  (is (= [ [] '[ (f 1) => 3 ] ] (separate-fact '[ (f 1) => 3 ])))
  (is (= [ [] '[ (f 1) => 3 ] ] (separate-fact '[ (against-background) (f 1) => 3 ])))
  (is (= [ '[(g) => 22] '[ (f 1) => 3 ] ]
	   (separate-fact '[ (against-background (g) => 22) (f 1) => 3 ])))
  (is (= [ '[(g) => 22 (h) => 3] '[ (f 1) => 3 ] ]
	   (separate-fact '[ (against-background (g) => 22)
			     (f 1) => 3
			     (against-background (h) => 3)]))))

(declare middlemost innermost)
(deftest against-background-defines-metaconstants
  (against-background [ (middlemost ...m...) => 33 ]
		      (fact
			(against-background (innermost ...i...) => 8)
			(+ (middlemost ...m...) (innermost ...i...)) => 41)))


(deftest wrapping-expression-with-form
  (is (= '(let [r 1] (* r 2))
	 (wrap '(let [r 1] ?form) '(* r 2)))))

; (set-namespace-pseudovariable :midje/wrappers '(let [r 1] ?form))
