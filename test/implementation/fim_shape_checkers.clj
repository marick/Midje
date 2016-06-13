(ns implementation.fim-shape-checkers
  (:require [midje.shape-checkers :as subject]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.checking.core :as core]
            [midje.util.ecosystem :refer [when-1-7+]]))

(when-1-7+

(fact
  (let [expected {[:a :b] [even? neg?]
                   :c      subject/required-path}

        good-actual {:a {:b 1} :c 2}
        broken-actual (vector {:a {:b 1}   :c 3}
                              {:a {:b -2}}
                              {:a {:b 2}   :c 3})]

    ( (subject/all-built-like expected) good-actual) => true
    (let [failure ( (subject/all-built-like expected) broken-actual)]
      failure => core/data-laden-falsehood?
      (first (:notes failure)) => #"should be")))

)
