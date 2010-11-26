(ns midje.sweet.t-metaconstants-test
  (:use [midje.sweet.metaconstants] :reload-all)
  (:use clojure.test)
  (:use midje.sweet)
  (:use midje.test-util)
)

(deftest metaconstants-begin-and-end-with-dots
  (is (metaconstant? '...foo...))
  (is (metaconstant? '.foo.))
  (is (not (metaconstant? 'foo)))
  (is (not (metaconstant? '.foo)))
  (is (not (metaconstant? 'foo.)))
  )

(defn claim-symbols [symbols]
  (doseq [metaconstant-symbol symbols]
    (is ((ns-interns *ns*) metaconstant-symbol))
    (is (= (var-get ((ns-interns *ns*) metaconstant-symbol))
	   metaconstant-symbol))))

(deftest metaconstants-are-automatically-defined
  (define-metaconstants '(fact (f ...form...) => 1
			  [:in ...vec...]
			  {:in ...map...}
			  #{:in ...set...}))
  (claim-symbols '(...form... ...vec... ...map... ...set...)))


(declare f)
(background (f ...one...) => 1 )
(against-background [ (f ...two...) => 2 ]
  (fact
    (+ (f ...one...) (f ...two...) (f ...three...))  => 6
    (against-background (f ...three...) => 3)))
(claim-symbols '(...one... ...two... ...three...))

