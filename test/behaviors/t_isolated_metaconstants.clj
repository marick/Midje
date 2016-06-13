(ns behaviors.t-isolated-metaconstants
  (:require [midje.sweet :refer :all]))


(defn verify-service [service]
 true)

(declare f)
 ;; In version with bug, the following is required to make this work.
 ;; (background (f) => 1)

(fact
 1 => 1
 (provided
   ..service.. =contains=> {:status 200}))

