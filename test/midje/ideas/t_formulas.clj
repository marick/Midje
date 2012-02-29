;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-formulas
  (:use midje.test-util
        midje.sweet))


;; Validation

(causes-validation-error #"There is no arrow in your formula form"
  (formula [a 1]))

(causes-validation-error #"There is no arrow in your formula form"
  (formula [a 1] 1))

(causes-validation-error #"There is no arrow in your formula form"
  (formula "vector fact" [a 1] (contains 3)))

(causes-validation-error #"There is no arrow in your formula form"
  (formula "vector fact" [a 1] (contains 3)))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" :not-vector 1 => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" [a 1 1] 1 => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" [] 1 => 1))
