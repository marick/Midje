(ns midje.ideas.reporting.t-levels
  (:use midje.ideas.reporting.level-defs
        midje.ideas.reporting.levels
        [midje sweet util test-util]
        [midje.clojure-test-facade :only [counters]])
  (:require [midje.config :as config]
            [midje.ideas.reporting.report :as report]))

(expose-testables midje.ideas.reporting.levels)

(config/with-augmented-config {:print-level :print-normally}

  
(facts "separating levels out of argument lists"
  (separate-print-levels [])
  => [ [] :print-normally nil ]
  (separate-print-levels [:print-nothing])
  => [[:print-nothing] :print-nothing nil]
  (separate-print-levels [:all :print-nothing])
  => [[:print-nothing] :print-nothing [:all]]
  (separate-print-levels [:print-nothing :all])
  => [[:print-nothing] :print-nothing [:all]]
  (let [number-form (names-to-levels :print-facts)]
        (separate-print-levels ['a number-form 'b])
        => [[number-form] number-form '[a b]])

  ;; checks for valid levels
  (separate-print-levels [500 'something]) => (throws Error #"500.*not.*valid")
  (separate-print-levels [:print-nothing :print-facts])
  => (throws Error #"extra.*print.*level"))


) ; restore print levels to user default
