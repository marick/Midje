(ns implementation.fim-shape-checkers
  (:require [midje.shape-checkers :as subject])
  (:use midje.sweet midje.test-util)
  (:require [midje.checking.core :as core]))

(def required-path structural-typing.type/required-path)

(fact
  (let [expected {[:a :b] [even? neg?]
                   :c      required-path}

        good-actual {:a {:b 1} :c 2}
        broken-actual (vector {:a {:b 1}   :c 3}
                              {:a {:b -2}}
                              {:a {:b 2}   :c 3})]

    ( (subject/all-built-like expected) good-actual) => true
    (let [failure ( (subject/all-built-like expected) broken-actual)]
      failure => core/data-laden-falsehood?
      (first (:notes failure)) => #"should be")))
