(ns midje.test-util
  (:use [clojure.test])
  (:use [midje.test-util])
)

(defmacro testable-privates [namespace & symbols]
  (let [make-form (fn [symbol] `(def ~symbol (intern '~namespace '~symbol)))
	forms (map make-form symbols)]
  `(do ~@forms))
)

