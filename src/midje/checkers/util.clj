;; -*- indent-tabs-mode: nil -*-
(ns midje.checkers.util
  (:use [midje.util.form-utils :only [classic-map?]]
        [midje.util.object-utils :only [name-object]]))

(defn named-as-call [name expected function]
  "Adds a string name that looks like a function call to
   a function's metadata under :name"
  (name-object function (format "(%s %s)" name expected)))
  
(def captured-exception-key "this Throwable was captured by midje:")

(defn captured-exception [e] 
  {captured-exception-key e})

(defn captured-exception? [value]
  (and (classic-map? value)
       (not (sorted? value)) ; Cannot dereference sorted maps with obj of diff. type than key.
       (value captured-exception-key)))

(defn captured-exception-value [captured-exception]
  (captured-exception captured-exception-key))

(defn throwable-with-class? [wrapped-throwable expected-class]
  (and (map? wrapped-throwable)
       (= expected-class (class (wrapped-throwable captured-exception-key)))))
