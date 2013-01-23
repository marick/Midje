(ns behaviors.t-default-prerequisites
  (:use [midje sweet test-util]))

(defn calls-nothing [])
(unfinished unused)

(against-background [(unused 3) => 4]
  (fact 
    (calls-nothing) => nil))
