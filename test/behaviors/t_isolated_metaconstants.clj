;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-isolated-metaconstants
  (:use [clojure.pprint])
  (:use [midje sweet]))


(defn verify-service [service]
 true)

(declare f g)
 (background (f) => 1 
             (g) => 33)

(fact
 1 => 1
 (provided
   ..service.. =contains=> {:status 200}))))
