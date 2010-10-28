(ns midje.t-semi-sweet-test
  (:use clojure.test)
  (:use [midje.semi-sweet] :reload-all)
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
	   #'midje.t-semi-sweet-test/faked-function))
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

(deftest fakes-with-overrides-test
  (let [expectation (fake (faked-function) => 2 :file-position 33)]
    (is (= 33 (expectation :file-position))))

  (let [filepos 33
	expectation (fake (faked-function) => 2 :file-position filepos)]
    (is (= 33 (expectation :file-position))))
  )

(deftest basic-not-called-test
  (let [expectation-0 (not-called faked-function)]

    (is (= (:function expectation-0)
           #'midje.t-semi-sweet-test/faked-function))
    (is (= (:call-text-for-failures expectation-0)
           "faked-function was called."))
    (is (= (deref (:count-atom expectation-0))
           0))

    (testing "arg-matchers are not needed" 
      (let [matchers (:arg-matchers expectation-0)]
        (is (nil? matchers)))
    )

    (testing "result supplied" 
	     (is (= ((:result-supplier expectation-0))
		    nil)))
    )
 )


(defn function-under-test [& rest]
  (apply mocked-function rest))
(defn no-caller [])

(deftest simple-examples
  (one-case "Without expectations, this is like 'is'."
    (expect (+ 1 3) => nil)
    (is (reported? 1 [{:type :mock-expected-result-failure
		      :actual 4
		      :expected nil}])))

  (one-case "A passing test so reports"
    (expect (+ 1 3) => 4)
    (is (reported? 1 [{:type :pass}])))
	    

  (one-case "successful mocking"
    (expect (function-under-test) => 33
       (fake (mocked-function) => 33))
    (is (no-failures?)))

  (one-case "successful mocking with not-called expected first"
    (expect (function-under-test) => 33
       (not-called no-caller)
       (fake (mocked-function) => 33))
    (is (no-failures?)))

  (one-case "successful mocking not-called expected last"
    (expect (function-under-test) => 33
       (fake (mocked-function) => 33)
       (not-called no-caller))
    (is (no-failures?)))


  (one-case "mocked calls go fine, but function under test produces the wrong result"
     (expect (function-under-test 33) => 12
	(fake (mocked-function 33) => (not 12) ))
     (is (reported? 1 [{:actual false
			:expected 12}])))

  (one-case "mock call supposed to be made, but wasn't (zero call count)"
    (expect (no-caller) => "irrelevant"
	    (fake (mocked-function) => 33))
    (is (reported? 2 [{:type :mock-incorrect-call-count}
		      {:type :mock-expected-result-failure}])))

  (one-case "mock call was not supposed to be made, but was (non-zero call count)"
     (expect (function-under-test 33) => "irrelevant"
             (not-called mocked-function))
    (is (reported? 2 [{:type :mock-incorrect-call-count}
		      {:type :mock-expected-result-failure}])))

  (one-case "call not from inside function"
     (expect (+ (mocked-function 12) (other-function 12)) => 12
	     (fake (mocked-function 12) => 11)
	     (fake (other-function 12) => 1))
     (is (no-failures?)))



  (one-case "call that matches none of the expected arguments"
     (expect (+ (mocked-function 12) (mocked-function 33)) => "result irrelevant because of earlier failure"
	     (fake (mocked-function 12) => "hi"))
    (is (reported? 2 [{:type :mock-argument-match-failure
		       :actual '(33)}
		      {:type :mock-expected-result-failure}])))

  (one-case "failure because one variant of multiply-mocked function is not called"
     (expect (+ (mocked-function 12) (mocked-function 22)) => 3
	     (fake (mocked-function 12) => 1)
	     (fake (mocked-function 22) => 2)
	     (fake (mocked-function 33) => 3))
    (is (reported? 2 [{:type :mock-incorrect-call-count
		       :expected-call "(mocked-function 33)" }
		      {:type :pass}]))) ; passes for wrong reason

  (one-case "multiple calls to a mocked function are perfectly fine"
     (expect (+ (mocked-function 12) (mocked-function 12)) => 2
	      (fake (mocked-function 12) => 1) ))
)

(deftest expect-with-overrides-test
  (one-case "can override entries in call-being-tested map" 
     (expect (function-under-test 1) => 33 :expected-result "not 33"
	       (fake (mocked-function 1) => "not 33"))
     (is (no-failures?)))

  (let [expected "not 33"]
    (expect (function-under-test 1) => 33 :expected-result expected
	    (fake (mocked-function 1) => "not 33"))))

(deftest duplicate-overrides---last-one-takes-precedence
  (let [expected "not 33"]
    (expect (function-under-test 1) => 33 :expected-result "to be overridden"
	                                  :expected-result expected
	  (fake (mocked-function 1) => "not 33"))
    (expect (function-under-test 1) => 33 :expected-result "to be overridden"
	                                  :expected-result expected
	  (fake (mocked-function 1) => 5 :result-supplier "IGNORED"
		                         :result-supplier (fn [] expected)))))
    

(deftest expect-returns-truth-value-test
  (is (true? (run-silently (expect (function-under-test 1) => 33
				   (fake (mocked-function 1) => 33)))))   
  (is (false? (run-silently (expect (function-under-test 1) => 33
				   (fake (mocked-function 2) => 33)))))  ; mock failure
  (is (false? (run-silently (expect (+ 1 1) => 33))))
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



(def actual-plus-one-is-greater-than (chatty-checker (> (inc actual) expected)))
(deftest chatty-function-awareness-test
  (one-case "chatty failures provide extra information"
     (expect (+ 1 1) => (actual-plus-one-is-greater-than 33))
     (is (reported? 1 [ {:type :mock-expected-result-functional-failure
			 :actual 2
			 :actual-processor #'inc
			 :processed-actual 3
			 :expected '(actual-plus-one-is-greater-than 33)} ])))
  (one-case "chatty checkers can be used inline"
     (expect (+ 1 1) => ( (chatty-checker (> (inc actual) expected)) 33))
     (is (reported? 1 [ {:type :mock-expected-result-functional-failure
			 :actual 2
			 :actual-processor #'inc
			 :processed-actual 3
			 :expected '( (chatty-checker (> (inc actual) expected)) 33)} ]))))

(declare chatty-prerequisite)
(defn chatty-fut [x] (chatty-prerequisite x))
(deftest chatty-functions-can-be-used-in-fake-args
  (one-case "chatty checkers can be used in fakes"
	    (expect (chatty-fut 5) => "hello"
		    (fake (chatty-prerequisite (actual-plus-one-is-greater-than 5)) => "hello"))
	    (is (no-failures?))))


(deftest fake-function-from-other-ns
  (let [myfun (fn [x] (list x))]
    (expect (myfun 1) => :list-called
            (fake (list 1) => :list-called))))

(use 'clojure.set)
(defn set-handler [set1 set2]
  (if (empty? (intersection set1 set2))
    set1
    (intersection set1 set2)))

(deftest fake-function-from-other-namespace-used-in-var
  (expect (set-handler 'set 'disjoint-set) => 'set
	  (fake (intersection 'set 'disjoint-set) => #{}))
  (expect (set-handler 'set 'overlapping-set) => #{'intersection}
	  (fake (intersection 'set 'overlapping-set) => #{'intersection}))
)


;; This test is rather indirect. The function under test returns a lazy seq
;; embedded within a top-level list. If the whole tree isn't evaluated, the
;; test will fail because the fake is never called. (Because fake results are
;; checked before final results, since that results in nicer output.)

(def testfun)
(defn lazy-seq-not-at-top-level []
  (list (map (fn [n] (testfun n)) [1])))

(deftest entire-trees-are-eagerly-evaluated
  (expect (lazy-seq-not-at-top-level) => '((32))
	  (fake (testfun 1) => 32)))

;; (deftest obeys-clojure-test-load-tests-var
;;   (binding [clojure.test/*load-tests* false]
;;     (expect (+ 1 1) => 333)))
