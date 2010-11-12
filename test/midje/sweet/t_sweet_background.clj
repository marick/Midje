(ns midje.sweet.t-sweet-background
  (:use clojure.test)
  (:use [midje.sweet.sweet-background])
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint))

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

;; (def *soothing* 2)

;; (println "and the answer IS!" (against-background
;; 			       (state [:around :once (with-binding {#'*soothing* 3} ?form)]
;; 				      [:before :each (blah...) (blah...)]
;; 				      state-producing-function)
;; 		       *soothing*))


;; (pprint (macroexpand '(against-background
;; 		       (state :once (with-binding {#'*soothing* 3} ?form))
;; 		       *soothing*)))
