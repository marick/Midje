(ns midje.unprocessed.t-unprocessed-internals
  (:use clojure.test)
  (:use [midje.unprocessed.unprocessed-internals] :reload-all)
  (:use [midje.semi-sweet :only [fake]])
  (:use [midje.util.checkers])
  (:use [midje.test-util]))

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

(declare f g)
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

(deftest function-aware-equality-test
  (is (falsey (function-aware-= 1 2)))
  (is (truthy (function-aware-= 1 odd?)))
  (let [checker (fn [expected] (chatty-checker [actual] (> (inc actual) expected)))]
    (is (falsey (function-aware-= 5 ((checker 5) 4)))))
)

(def root "root")
(def unbound!)

(deftest with-altered-roots-test
  (is (= "root" root))
  (is (= "override" (with-altered-roots {(var root) "override"} root)))
  (is (= "root" root))

  ;; works with lexical scope
  (let [appendage " ROOT!"]
    (is (= "override ROOT!" (with-altered-roots {#'root "override"} (str root appendage)))))
  (is (= "root" root))

  ;; works with unbound variables
  (is (not (bound? #'unbound!)))
  (is (= "override" (with-altered-roots {#'unbound! "override"} unbound!)))
  (is (not (bound? #'unbound!)))

  ;; works with throwables
  (try
    (with-altered-roots {#'root "override"} (throw (Throwable. root)))
    (catch Throwable ex
      (is (= "override" (.getMessage ex)))))
  (is (= "root" root)))

