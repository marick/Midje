;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-building
  (:use [midje sweet test-util])
  (:require [midje.midje-forms.building :as b])
  (:require [clojure.zip :as zip])
)

(fact "metaconstants can be created to stand in for an expression"
  (b/forgetting-unfolded-prerequisites
    (b/metaconstant-for-form '(g)) => '...g-value-1...
    (b/metaconstant-for-form '(g)) => '...g-value-2...
    (b/metaconstant-for-form '(h)) => '...h-value-1...

    "Not fooled by namespaces"
    (b/metaconstant-for-form '(b/metaconstant-for-form))
    => '...metaconstant-for-form-value-1...))

(fact "make-fake's result has the line number of the arrow form"
  (let [args `( ~(at-line 789 '(f 1)) => 3)]
    (:line (meta (b/make-fake args))) => 789))

     
