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
  (is (chatty-checker-falsehood? (tag-as-chatty-falsehood [5]))))

(deftest chatty-checker-function-test
  (let [actual-plus-one-equals (chatty-checker* #'inc #'=)]
    (is (chatty-checker? (actual-plus-one-equals 4)))
    (is (= true ((actual-plus-one-equals 4) 3)))
    (let [result ((actual-plus-one-equals 4) 4)]
      (is (chatty-checker-falsehood? result))
      (is (= {:actual 4
	      :actual-processor #'inc
	      :processed-actual 5}
	     result))))
  (let [actual-plus-one-greater-than (chatty-checker* #'inc #'>)]
    (is (= true ((actual-plus-one-greater-than 5) 5)))
    (is (chatty-checker-falsehood? ((actual-plus-one-greater-than 5) 4)))))

(deftest chatty-checker-macro-test
  (let [actual-plus-one-greater-than (chatty-checker (> (inc actual) expected))]
    (is (chatty-checker? (actual-plus-one-greater-than 5)))
    (is (= true ((actual-plus-one-greater-than 5) 5)))
    (is (chatty-checker-falsehood? ((actual-plus-one-greater-than 5) 4)))))
