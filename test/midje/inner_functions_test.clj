(ns midje.inner-functions-test
  (:use clojure.test)
  (:use [midje.semi-sweet] :reload-all)
  (:use [midje.inner-functions] :reload-all)
  (:use [midje.test-util]))

(testable-defn outer [a b]
  (let [summer (fn [arg] (+ a arg))
	multiplier (fn [] (* (summer b) (summer b)))]
    (multiplier)))

(deftest main-example
; (expect (within (outer 1 2) (summer)) => 3)
; (expect (within (outer 1 2) (multiplier)) => 6)
)
