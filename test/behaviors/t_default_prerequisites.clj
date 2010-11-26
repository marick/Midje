(ns behaviors.t-default-prerequisites
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(defn calls-nothing [])
(unfinished unused)

(fact "background prerequisites don't have to be used"
  (expect (calls-nothing) => nil
	  (fake (unused) => 3 :type :background)))

