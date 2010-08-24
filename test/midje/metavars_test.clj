(ns midje.sweet-internals-test
  (:use [midje.sweet] :reload-all)
  (:use [midje.fact-body-transformation] :reload-all)
  (:use clojure.test)
  (:use midje.test-util)
)

(deprivatize midje.sweet frozen-runs no-expectations-follow? define-metavars metavar?)

(defmacro frozen-run! [actual call-being-tested _ expected-result]
  `(is (= '~call-being-tested (~actual :function-under-test)))
  `(is (= '~expected-result (~actual :expected-result)))
)

(defmacro expectation! [actual expected-call-form _ expected-mocked-result]
  `(is (= '~expected-call-form (first ~actual)))
  `(is (= '~expected-mocked-result (nth ~actual 2)))
)

(defmacro n! [n seq]
  `(is (= ~n (count ~seq))))

(defmacro one! [seq]
  `(n! 1 ~seq))


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

(deftest when-no-expectations-follow?
  (is (no-expectations-follow? []))
  (is (no-expectations-follow? '[not-a-seq]))
  (is (no-expectations-follow? '[ (f 3) => 4]))
  (is (false? (no-expectations-follow? '[ (provided (f 3) => 4)])))
)

(deftest forms-without-expectations
  (let [actual (frozen-runs '((f 1) => 3))]
    (one! actual)
    (frozen-run! (actual 0) (f 1) => 3))

  (let [actual (frozen-runs '((f 2) => 3
			      (f 3) => (+ 3 3)))]
    (n! 2 actual)
    (frozen-run! (actual 0) (f 2) => 3)
    (frozen-run! (actual 1) (f 3) => (+ 3 3)))
)

(deftest parse-provided-forms-tests
  (let [actual (vec (frozen-runs '(
				   (f 1) => 3
				   (provided
				    (g 1) => 3)
				   )))]
    (one! actual)
    (frozen-run! (actual 0) (f 1) => 3)

    (let [expectations ((actual 0) :expectations)]
      (one! expectations)
      (expectation! (expectations 0) (g 1) => 3)
      )
))


