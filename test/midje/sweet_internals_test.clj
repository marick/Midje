(ns midje.sweet-internals-test
  (:use [midje.sweet] :reload-all)
  (:use clojure.test)
)
(defn test-ns-hook [] (run-tests 'midje.sweet))

(in-ns 'midje.sweet)
(use 'clojure.test)
(use 'midje.test-util)

(defmacro frozen-run! [actual call-being-tested _ expected-result]
  `(is (= ~actual
	   {:function-under-test '~call-being-tested :expected-result '~expected-result })))

(defmacro n! [n seq]
  `(is (= ~n (count ~seq))))

(defmacro one! [seq]
  `(n! 1 ~seq))
  
(deftest parse-simple-frozen-run-forms-tests
  (let [actual (vec (parse-forms '((f 1) => 3)))]
    (one! actual)
    (frozen-run! (actual 0) (f 1) => 3))

  (let [actual (vec (parse-forms '((f 2) => 3
				   (f 3) => (+ 3 3))))]
    (n! 2 actual)
    (frozen-run! (actual 0) (f 2) => 3)
    (frozen-run! (actual 1) (f 3) => (+ 3 3)))
)

(deftest parse-provided-forms-tests
  (let [actual (vec (parse-forms '(
				   (f 1) => 3
				   (provided
				    (g 1) => 3)
				   )))]
    (one! actual)
    (frozen-run! (actual 0) (f 1) => 3)
)
)


