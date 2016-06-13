(ns behaviors.t-default-prerequisites
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]))

(defn calls-nothing [])
(unfinished unused)

(against-background [(unused 3) => 4]
  (fact 
    (calls-nothing) => nil))
