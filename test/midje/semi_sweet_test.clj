(ns midje.semi-sweet-test
  (:use [midje.semi-sweet] :reload-all)
  (:use [midje.checkers])
  (:use [clojure.test])
  (:use [midje.test-util]))

(only-mocked faked-function mocked-function other-function)

(deftest only-mocked-test
  (try
     (faked-function)
     (is false "Function didn't raise.")
     (catch Error e)))


(deftest basic-fake-test
  (let [some-variable 5
	previous-line-position (file-position 1)
	expectation-0 (fake (faked-function) => 2)
	expectation-1 (fake (faked-function some-variable) => (+ 2 some-variable))
	expectation-2 (fake (faked-function 1 some-variable) => [1 some-variable])]

    (is (= (:function expectation-0)
	   #'midje.semi-sweet-test/faked-function))
    (is (= (:call-text-for-failures expectation-1)
	   "(faked-function some-variable)"))
    (is (= (deref (:count-atom expectation-0))
	   0))

    (testing "argument matching" 
	     (let [matchers (:arg-matchers expectation-0)]
	       (is (= (count matchers) 0)))

	     (let [matchers (:arg-matchers expectation-1)]
	       (is (= (count matchers) 1))
	       (is (truthy ((first matchers) 5)))
	       (is (falsey ((first matchers) nil))))

	     (let [matchers (:arg-matchers expectation-2)]
	       (is (= (count matchers) 2))
	       (is (falsey ((first matchers) 5)))
	       (is (truthy ((first matchers) 1)))
	       (is (truthy ((second matchers) 5)))
	       (is (falsey ((second matchers) 1))))
    )

    (testing "result supplied" 
	     (is (= ((:result-supplier expectation-0))
		    2))
	     (is (= ((:result-supplier expectation-1))
		    (+ 2 some-variable)))
	     (is (= ((:result-supplier expectation-2))
		    [1 some-variable]))
    )
    )
)

(defn function-under-test [& rest]
  (apply mocked-function rest))
(defn no-caller [])

(deftest simple-examples
  (one-case "Without expectations, this is like 'is'."
    (expect (+ 1 3) => nil)
    (is (last-type? :mock-expected-result-failure))
    (is (= (:actual (last @reported)) 4))
    (is (= (:expected (last @reported)) nil)))

  (one-case "A passing test so reports"
    (expect (+ 1 3) => 4)
    (is (last-type? :pass)))
	    

  (one-case "successful mocking"
    (expect (function-under-test) => 33
       (fake (mocked-function) => 33))
    (is (no-failures?)))


  (one-case "mocked calls go fine, but function under test produces the wrong result"
     (expect (function-under-test 33) => 12
	(fake (mocked-function 33) => (not 12) ))
     (is (= (:actual (last @reported)) false))
     (is (= (:expected (last @reported)) 12)))

  (one-case "mock call supposed to be made, but wasn't (zero call count)"
    (expect (no-caller) => "irrelevant"
       (fake (mocked-function) => 33))
    (is (last-type? :mock-incorrect-call-count))
    (is (only-one-result?)))


  (one-case "call not from inside function"
     (expect (+ (mocked-function 12) (other-function 12)) => 12
	     (fake (mocked-function 12) => 11)
	     (fake (other-function 12) => 1))
     (is (no-failures?)))



  (one-case "call that matches none of the expected arguments"
     (expect (+ (mocked-function 12) (mocked-function 33)) => "result irrelevant because of earlier failure"
	     (fake (mocked-function 12) => "hi"))
     (is (last-type? :mock-argument-match-failure))
     (is (only-one-result?)))



  (one-case "failure because one variant of multiply-mocked function is not called"
     (expect (+ (mocked-function 12) (mocked-function 22)) => 3
	     (fake (mocked-function 12) => 1)
	     (fake (mocked-function 22) => 2)
	     (fake (mocked-function 33) => 3))
     (is (last-type? :mock-incorrect-call-count))
     (is (only-one-result?)))

  (one-case "multiple calls to a mocked function are perfectly fine"
     (expect (+ (mocked-function 12) (mocked-function 12)) => 2
	      (fake (mocked-function 12) => 1) ))
)


  
(deftest function-awareness-test
  (one-case "expected results can be functions"
     (expect (+ 1 1) => even?)
     (is (no-failures?)))
  (let [myfun (fn [] 33)
	funs [myfun]]
    (one-case "exact function matches can be checked with exactly"
       (expect (first funs) => (exactly myfun))
       (is (no-failures?))))

  (one-case "mocked function argument matching uses function-aware equality"
     (expect (function-under-test 1 "floob" even?) => even?
	     (fake (mocked-function odd? anything (exactly even?)) => 44))
     (is (no-failures?)))
)
