(ns midje.sweet.t-background
  (:use clojure.test)
  (:use [midje.sweet.background])
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util]))

(deftest separating-background-forms
  (is (= [ [] [] ]  (separate [])))
  (is (= '[ [] [(fact) (+ 1 1)] ]
	 (separate '[(fact) (+ 1 1)])))
  (is (= '[ [(midje.semi-sweet/fake (f 1) => 2 :type :background)] [] ]
	 (separate '[(f 1) => 2])))
  (is (= '[ [ (midje.semi-sweet/fake (f 1) => 2 :type :background)
	      (midje.semi-sweet/fake (f 2) => 33 :type :background) ]
	    [ (fact (f 1) => 2)
	      (+ 1 1)                 ]]
	 (separate '[   (f 1) => 2
			(fact (f 1) => 2)
			(f 2) => 33
			(+ 1 1)        ])))
)
