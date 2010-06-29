
(ns semi-sweet-simple.core-test
  (:use [semi-sweet-simple.core] :reload-all)
  (:use [clojure.test])
  (:use [midje.semi-sweet])
)

;; This is an example of the Midje version of a clojure.test test that would 
;; look like this:
;;      (is (= (+ 1 1) 2))
;;
;; Midje uses the clojure.test reporting mechanism, so that you can continue to
;; use tools that assume clojure.test.
(deftest example-of-a-simple-equality-test
  (expect (+ 1 1) => 2))

;; Failing tests should look familiar:
;;     FAIL at (core_test.clj:19)
;;     expected: 3
;;       actual: 4
(deftest example-of-a-simple-equality-test-failure
  (expect ( #(+ 1 %) 3) => 3))

;; You can also use functions on the right-hand side. In that case,
;; the actual result is passed as the function's single argument.

(deftest example-of-a-function-as-right-hand-side
  (expect ( #(+ 1 %) 3) => odd?))


