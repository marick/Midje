;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-extended-equality
  (:use midje.sweet
        midje.checkers.extended-equality
        midje.test-util))

(facts "about extended equality"
  (extended-= 1 2) => falsey
  (extended-= 1 odd?) => truthy

  (let [checker (fn [expected] (chatty-checker [actual] (> (inc actual) expected)))]
    (extended-= 5 ((checker 5) 4)) => falsey)

  "regexps"
  (extended-= #"a*b+" #"a*b+") => truthy
  (extended-= #"a*b+" #"a*b") => falsey
  (extended-= "BEGIN aab END" #"a*b+") => truthy
  (extended-= "BEGIN bb END" #"ab+") => falsey

  ;; When searching for unordered comparisons, you might get exceptions.
  ;; Count those as false.
  (extended-= nil odd?) => falsey)

(fact "extended equality can be applied to lists"
  (extended-list-= [] []) => truthy
  (extended-list-= [1] [1]) => truthy
  (extended-list-= ['()] [seq?]) => truthy
  (extended-list-= ['() 1] [seq? seq?]) => falsey)



