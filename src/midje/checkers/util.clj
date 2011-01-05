;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.util)

(defn tag-as-checker [function]
  (vary-meta function merge {:midje/checker true}))
  
(defn named [name expected function]
  "Adds a string name that looks like a function call to
   a functions metadata under :name"
  (with-meta function
        {:name (format "(%s %s)" name expected)}))
  

(def captured-exception-key "this Throwable was captured by midje:")
(defn captured-exception [e] {captured-exception-key e})
(defn captured-exception? [value] (and (map? value) (value captured-exception-key)))

(defn throwable-with-class? [wrapped-throwable expected-class]
  (and (map? wrapped-throwable)
       (= expected-class (class (wrapped-throwable captured-exception-key)))))

