;; -*- indent-tabs-mode: nil -*-

(ns midje.t-expect
  (:use [midje.expect]
	midje.sweet
	midje.test-util)
  (:require [clojure.zip :as zip])
)

(fact "can position so loc is the entire expect form"
  (let [z (zip/seq-zip '(expect (f 1) => (+ 1 1)))
	finds-enclosing (fn [loc] (= (zip/node (zip/down loc)) 'expect))]
    (up-to-full-expect-form z) => finds-enclosing
    (up-to-full-expect-form (zip/down z)) => finds-enclosing
    (up-to-full-expect-form (-> z zip/down zip/rightmost)) => finds-enclosing
    (up-to-full-expect-form (-> z zip/down zip/rightmost zip/down)) => finds-enclosing))

