(ns midje.background.t-midjcoexpand
  (:use midje.sweet)
  (:use midje.background.midjcoexpand))

;(fact "facts expand into expect forms, wrapped if necessary"
;  (midjcoexpand '(fact (f 1) => b)) => '(do (expect (f 1) => b)))
