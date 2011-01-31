;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-util
  (:use midje.sweet
        midje.checkers.util
        midje.test-util))

(defrecord R [a])

(fact "captured exceptions can be recognized"
  (captured-exception? (captured-exception (Throwable.))) => truthy
  "and are not fooled by maps or records"
  (captured-exception? {}) => falsey
  (captured-exception? (R. 1)) => falsey)

(defn user-defined-checker 
  {:midje/checker true}
  [actual]
  false)

(fact "checkers can be recognized"
  'midje.checkers.simple/truthy => checker?
  #'midje.checkers.simple/truthy => checker?
  (checker? 'clojure.core/odd?) => falsey
  (checker? #'clojure.core/odd?) => falsey

  ;; this is required because plain symbols don't get namespaces. They *do*
  ;; have namespaces when they come out of macroexpansion.
  (let [this-namespace-checker-in-use `((f user-defined-checker 3) => 3)]
    (second (first this-namespace-checker-in-use)) => checker?)
  #'user-defined-checker => checker?

  "sloop" => (fn [actual] (= "sloop" actual)) ; an inline checker
  (checker? (fn [actual] (= "sloop" actual))) => falsey
  (tag-as-checker (fn [actual] (= "sloop" actual))) => checker?

  (checker? "blah") => falsey
  (checker? 1) => falsey)

