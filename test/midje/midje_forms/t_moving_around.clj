(ns midje.midje-forms.t-moving-around
  (:use [midje.midje-forms.moving-around]
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

(defn node [expected] (fn [actual] (= expected (zip/node actual))))

(fact "can position loc at rightmost leaf"
  (let [z (zip/seq-zip '(a b "leaf"))]
    (skip-to-rightmost-leaf (zip/down z)) => (node "leaf"))

    (let [z (zip/seq-zip '(111 (a => [1 2 '(3)]) (("leaf"))))]
      (skip-to-rightmost-leaf (zip/down z)) => (node "leaf")))


