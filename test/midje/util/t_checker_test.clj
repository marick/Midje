(ns midje.util.t-checker-test
  (:use clojure.test)
  (:use [midje.semi-sweet] :reload-all)
  (:use [midje.test-util]))

(deftest truthy-test
  (expect true => truthy)
  (expect 1 => truthy)
  (expect (truthy false) => false)
  (expect (truthy nil) => false))

(deftest falsey-test
  (expect false => falsey)
  (expect nil => falsey)
  (expect (falsey true) => false)
  (expect (falsey 1) => false))

(deftest anything-test
  (expect true => anything)
  (expect false => anything)
  (expect even? => anything))

(deftest exactly-test
  (expect true => (exactly true))
  (expect ( (exactly 2) 2) => truthy)
  (expect ( (exactly 1) 2) => falsey)
  (expect even? => (exactly even?)))

(deftest in-any-order-test
  (expect [] => (in-any-order []))
  (expect [1] => (in-any-order [1]))
  (expect '(2 1) => (in-any-order [1 2]))
  (expect [ {:a 1} {:b 2} ] => (in-any-order [{:b 2} {:a 1}]))

  (expect ( (in-any-order [1 2]) [1 2 3]) => falsey)
  (expect ( (in-any-order [1 2]) [1]) => falsey)
  (expect ( (in-any-order [1 2]) [1 3]) => falsey)
  
  (expect ( (in-any-order [1 2 2 3]) [1 2 3 3]) => falsey)
  (expect ( (in-any-order [2 1 3 2]) [1 2 3 3]) => falsey)
  )

(deftest map-containing-test
  (expect {:a 1 :b 2} => (map-containing {:a 1 :b 2}))
  (expect {:a 1 :b 2 :c 3} => (map-containing {:a 1 :b 2}))

  (expect ( (map-containing {:a 1 :b 2})  {:a 1}) => falsey)
  (expect ( (map-containing {:a 1 :b 2})  {:a 1 :b 3}) => falsey)
  )

(deftest only-maps-containing-test
  (expect ( (only-maps-containing {:a 1 :b 2}) [{:a 1 :b 2} {:extra true}]) => falsey)
  (expect ( (only-maps-containing {:a 1 :b 2}  {:extra true}) [{:a 1 :b 2}]) => falsey)

  (expect [{:a 1 :b 2} {:extra 1}] => (only-maps-containing {:extra 1} {:a 1}))
  (expect [{:a 1 :b 2} {:a 1 :b 22}] => (only-maps-containing {:b 2} {:b 22}))
  (expect [{:a 1 :b 2} {:a 1 :b 22}] => (only-maps-containing [{:b 2} {:b 22}]))
  (expect ( (only-maps-containing {:b 2} {:b 22}) [{:b 2} {:b 33}]) => falsey))

(deftest maps-containing-test
  (expect ( (maps-containing {:a 1 :b 2}  {:extra true}) [{:a 1 :b 2}]) => falsey)

  (expect [{:a 1 :b 2} {:extra 1}] => (maps-containing {:extra 1} {:a 1}))
  (expect [{:a 1 :b 2} {:a 1 :b 22}] => (maps-containing {:b 2} {:b 22}))
  (expect [{:a 1 :b 2} {:a 1 :b 22} {:a 1 :b 33}] => (maps-containing {:b 2} {:b 22}))
  (expect [{:a 1 :b 2} {:a 1 :b 22} {:a 1 :b 33}] => (maps-containing [{:b 2} {:b 22}]))
  (expect ( (maps-containing {:b 2} {:b 22}) [{:b 2} {:b 33}]) => falsey))


(defn throw-exception
  ([] (throw (NullPointerException.)))
  ([message] (throw (Error. message)))
)

(deftest throwing-exceptions-test
  (one-case "detects correctly thrown exception"
    (expect (throw-exception) => (throws NullPointerException))
    (is (no-failures?)))
  (one-case "rejects incorrectly thrown exception"
    (expect (throw-exception "throws Error") => (throws NullPointerException))
    (is (reported? 1 [{:type :mock-expected-result-functional-failure}])))
  (one-case "detects correct message"
    (expect (throw-exception "hi") => (throws Error "hi"))
    (is (no-failures?)))
  (one-case "detects incorrect message"
     (expect (throw-exception "throws Error") => (throws Error "bye"))
     (is (reported? 1 [{:type :mock-expected-result-functional-failure}])))
  )

(deftest chatty-utility-tests
  (is (chatty-checker-falsehood? (tag-as-chatty-falsehood [5])))

  (is (not (chatty-worth-reporting-on? 1)))
  (is (not (chatty-worth-reporting-on? '())))
  (is (chatty-worth-reporting-on? '(f)))
  (is (chatty-worth-reporting-on? ''(f)))
  (is (not (chatty-worth-reporting-on? '[f]))))

(deftest unteasing-multi-level-arglist-into-var-references
  (is (= (chatty-untease 'g-101 '()) [[] []]))
  (is (= (chatty-untease 'g-101 '(1 (f) 33 (+ 1 2)))
	 [ '( (f) (+ 1 2))  '(1 (g-101 0) 33 (g-101 1))  ]))
  )
  

(deftest chatty-checker-test
  (let [actual-plus-one-equals-4 (chatty-checker [actual] (= (inc actual) 4))]
    (is (chatty-checker? actual-plus-one-equals-4))
    (is (= true (actual-plus-one-equals-4 3)))
    (let [result (actual-plus-one-equals-4 4)]
      (is (chatty-checker-falsehood? result))
      (is (= {:actual 4
  	      :intermediate-results [ ['(inc actual) 5] ] }
  	     result))))

  (let [no-longer-limited-form (chatty-checker [actual] (= (inc actual) 4 (+ 2 actual)))]
    (is (chatty-checker? no-longer-limited-form))
    (let [result (no-longer-limited-form 4)]
      (is (chatty-checker-falsehood? result))
      (is (= {:actual 4
  	      :intermediate-results [ ['(inc actual) 5] ['(+ 2 actual) 6] ]}
  	     result)))))

    
