(ns midje.semi-sweet-test
  (:use [midje.semi-sweet] :reload-all)
  (:use [midje.checkers])
  (:use [clojure.test])
  (:use [midje.test-util]))


(testable-privates midje.semi-sweet
		   pairs matching-args? find-matching-call eagerly
		   unique-function-vars binding-map
)


(deftest pairs-test
  (is (= (pairs [:a :b :c] [1 2 3])
	 [ [:a 1] [:b 2] [:c 3] ]))
)

;; Justification for use of eagerly
(def counter (atom 1))
(def mocked-function-produces-next-element inc)

(defn function-under-test-produces-a-lazy-list []
  (iterate mocked-function-produces-next-element 1))

(defn mock-use []
  (binding [mocked-function-produces-next-element (fn [n] (swap! counter inc) (inc n))]
    (eagerly (take 5 (function-under-test-produces-a-lazy-list)))))

(deftest eagerly-forces-evaluation-test
  (mock-use)
  (is (= (deref counter) 5))
)

(deftest eagerly-allows-non-sequences
  (is (= (eagerly 3) 3))
)


(deftest matching-args-test
  (is (truthy (matching-args? [] [])))
  (is (truthy (matching-args? ['()] [seq?])))
  (is (falsey (matching-args? ['() 1] [seq? seq?])))
)

(def faked-function)
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

(deftest unique-function-vars-test 
  (let [expectations [ (fake (f 1) => 2)
		       (fake (f 2) => 4)
		       (fake (g) => 3)] ]
    (is (truthy ((in-any-order [#'f #'g]) (unique-function-vars expectations)))))
)

(deftest find-matching-call-test
  (let [expectation {:function 'f
	             :arg-matchers [ odd? ] }]
    (is (falsey (find-matching-call 'g [3] [expectation])))
    (is (falsey (find-matching-call 'f [3 3] [expectation])))
    (is (falsey (find-matching-call 'f [4] [expectation])))
    (is (= (find-matching-call 'f [3] [expectation])
	   expectation))
  )
)

(deftest binding-map-test 
  (let [expectations [(fake (f 1) => 3)
		      (fake (g 1) => 4)
		      (fake (f 2) => 5)]
	result-map (binding-map expectations)
	count-checker (fn [val-f-1 val-g-1 val-f-2]
			  (is (= val-f-1 (deref (:count-atom (first expectations)))))
			  (is (= val-g-1 (deref (:count-atom (second expectations)))))
			  (is (= val-f-2 (deref (:count-atom (nth expectations 2))))))]

    (call-faker (var f) [1] expectations)
    (count-checker 1 0 0)
    (call-faker (var f) [1] expectations)
    (count-checker 2 0 0)
    (call-faker (var f) [2] expectations)
    (count-checker 2 0 1)
    (call-faker (var g) [1] expectations)
    (count-checker 2 1 1)
    )
)
