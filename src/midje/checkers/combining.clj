(ns ^{:doc "Checkers that combine other checkers."}
  midje.checkers.combining
  (:use [midje.checkers.defining]
        [midje.checkers.chatty]
        [midje.util.backwards-compatible-utils :only [every-pred-m some-fn-m]]

        [midje.checkers.extended-falsehood]))

(defn every-checker 
  "Combines multiple checkers into one checker that passes 
   when all component checkers pass."
  [& checkers]
  (checker [& args] 
    (not-any? #(extended-false? (apply % args)) checkers)))

(defn some-checker 
  "Combines multiple checkers into one checker that passes 
   when any of the component checkers pass."
  [& checkers]
  (as-checker (apply some-fn-m checkers)))


