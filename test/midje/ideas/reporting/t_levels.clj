(ns midje.ideas.reporting.t-levels
  (:use midje.ideas.reporting.levels)
  (:use [midje sweet util test-util]))
(expose-testables midje.ideas.reporting.levels)

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
  (-> :print-facts names-to-levels levels-to-names) => :print-facts)
  
