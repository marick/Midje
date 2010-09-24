(ns midje.sweet.t-metaconstants-test
  (:use [midje.sweet.metaconstants] :reload-all)
  (:use clojure.test)
  (:use midje.test-util)
)

(deftest metaconstants-begin-and-end-with-dots
  (is (metaconstant? '...foo...))
  (is (metaconstant? '.foo.))
  (is (not (metaconstant? 'foo)))
  (is (not (metaconstant? '.foo)))
  (is (not (metaconstant? 'foo.)))
  )

(deftest metaconstants-are-automatically-defined
  (define-metaconstants '(fact (f ...form...) => 1
			  [:in ...vec...]
			  {:in ...map...}
			  #{:in ...set...}))
  (doseq [metaconstant-symbol '(...form... ...vec... ...map... ...set...)]
    (is ((ns-interns *ns*) metaconstant-symbol))
    (is (= (var-get ((ns-interns *ns*) metaconstant-symbol))
	   metaconstant-symbol)))
)

