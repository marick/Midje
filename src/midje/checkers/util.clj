;; -*- indent-tabs-mode: nil -*-
(ns midje.checkers.util
  (:use [midje.util.form-utils :only [classic-map?]]))

(defn named [name expected function]
  "Adds a string name that looks like a function call to
   a functions metadata under :name"
  (vary-meta function assoc :name (format "(%s %s)" name expected)))
  
(def captured-exception-key "this Throwable was captured by midje:")

(defn captured-exception [e] 
  {captured-exception-key e})

(defn captured-exception? [value]
  (and (classic-map? value)
       (value captured-exception-key)))

(defn captured-exception-value [captured-exception]
  (captured-exception captured-exception-key))

(defn throwable-with-class? [wrapped-throwable expected-class]
  (and (map? wrapped-throwable)
       (= expected-class (class (wrapped-throwable captured-exception-key)))))
