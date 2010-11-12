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

;; (def *soothing* 2)

;; (println "and the answer IS!" (against-background
;; 			       (state [:around :once (with-binding {#'*soothing* 3} ?form)]
;; 				      [:before :each (blah...) (blah...)]
;; 				      state-producing-function)
;; 		       *soothing*))


;; (pprint (macroexpand '(against-background
;; 		       (state :once (with-binding {#'*soothing* 3} ?form))
;; 		       *soothing*)))
