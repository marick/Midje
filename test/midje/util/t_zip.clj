;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-zip
  (:use [midje.util.zip]
	midje.sweet
	midje.test-util)
  (:require [clojure.zip :as zip])
)

(defn node [expected] (fn [actual] (= expected (zip/node actual))))

(fact "can position loc at rightmost leaf"
  (let [z (zip/seq-zip '(a b "leaf"))]
    (skip-to-rightmost-leaf (zip/down z)) => (node "leaf"))

    (let [z (zip/seq-zip '(111 (a => [1 2 '(3)]) (("leaf"))))]
      (skip-to-rightmost-leaf (zip/down z)) => (node "leaf")))


