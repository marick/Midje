(ns implementation.line-numbers.fim-deftest
  (:use clojure.test
        midje.sweet
        midje.test-util)
  (:require [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.state :as state]
            [midje.config :as config]))

(deftest fails
  ;; For some reason, clojure.test runs in an environment without the right binding for *file*
  (binding [*file* "NO_SOURCE_PATH"]
    (state/with-isolated-output-counters ; we only want the output
      (fact (+ 1 1) => 3))))

(fact
  (let [test-result (ctf/run-tests [*ns*])]
    (:lines test-result) => (contains #"fim_deftest.clj:13")))

(alter-meta! #'fails dissoc :test) ;; To keep from spewing failure output
