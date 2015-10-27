(ns implementation.line-numbers.fim-deftest
  (:use clojure.test
        midje.sweet
        midje.test-util)
  (:require [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.state :as state]
            [midje.config :as config]))

(deftest fails
  (state/with-isolated-output-counters ; we only want the output
    (fact (+ 1 1) => 3)))

(fact "In clojure 1.8, a fact within `deftest` no longer gets the filename right"
  (let [test-result (ctf/run-tests [*ns*])]
    (:lines test-result) => (contains #"fim_deftest.clj:11")))

(alter-meta! #'fails dissoc :test) ;; To keep from spewing failure output
