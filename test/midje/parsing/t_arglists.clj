(ns midje.parsing.t-arglists
  (:use midje.sweet
        midje.test-util
        midje.parsing.arglists)
  (:require [midje.emission.levels :as print-levels]))

(facts "separating levels out of argument lists"
  (separate-print-levels [] :print-normally)
  => [ [] :print-normally nil ]

  (separate-print-levels [:print-nothing] ..irrelevant..)
  => [[:print-nothing] :print-nothing nil]

  (separate-print-levels [:all :print-nothing] ..irrelevant..)
  => [[:print-nothing] :print-nothing [:all]]

  (separate-print-levels [:print-nothing :all] ..irrelevant..)
  => [[:print-nothing] :print-nothing [:all]]
  
  (let [number-form (print-levels/names-to-levels :print-facts)]
    (separate-print-levels ['a number-form 'b] ..irrelevant..)
    => [[number-form] number-form '[a b]])
  
  ;; checks for valid levels
  (separate-print-levels [500 'something] ..irrelevant..)
  => (throws Error #"500.*not.*valid")

  (separate-print-levels [:print-nothing :print-facts] ..irrelevant..)
  => (throws Error #"extra.*print.*level"))
