(ns midje.sweet.t-metavars-test
  (:use [midje.sweet.metavars] :reload-all)
  (:use clojure.test)
  (:use midje.test-util)
)

(deftest metavars-begin-and-end-with-dots
  (is (metavar? '...foo...))
  (is (metavar? '.foo.))
  (is (not (metavar? 'foo)))
  (is (not (metavar? '.foo)))
  (is (not (metavar? 'foo.)))
  )

(deftest metavars-are-automatically-defined
  (define-metavars '(fact (f ...form...) => 1
			  [:in ...vec...]
			  {:in ...map...}
			  #{:in ...set...}))
  (doseq [metavar-symbol '(...form... ...vec... ...map... ...set...)]
    (is ((ns-interns *ns*) metavar-symbol))
    (is (= (var-get ((ns-interns *ns*) metavar-symbol))
	   metavar-symbol)))
)

