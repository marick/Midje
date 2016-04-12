(ns midje.emission.t-levels
  (:require [midje.emission.levels :refer :all]
            [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.config :as config]))

(facts "about levels"
  (-> -2 levels-to-names names-to-levels) => -2
  (-> -1 levels-to-names names-to-levels) => -1
  (-> 0 levels-to-names names-to-levels) => 0
  (-> 1 levels-to-names names-to-levels) => 1
  (-> 2 levels-to-names names-to-levels) => 2

  (-> :print-nothing names-to-levels levels-to-names) => :print-nothing
  (-> :print-no-summary names-to-levels levels-to-names) => :print-no-summary
  (-> :print-normally names-to-levels levels-to-names) => :print-normally
  (-> :print-namespaces names-to-levels levels-to-names) => :print-namespaces
  (-> :print-facts names-to-levels levels-to-names) => :print-facts

  (validate-level! :print-namespaces) => anything
  (validate-level! :print-namespace ) => (throws #":print-namespace.*valid")
  (validate-level! 500) => (throws #"500.*valid")

  (normalize :print-nothing) => (names-to-levels :print-nothing)
  (normalize :a-mistake) => (throws ":a-mistake is not a valid :print-level.")
  (normalize 800) => (throws "800 is not a valid :print-level."))
