;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.t-unfinished
  (:use [midje sweet test-util]))

(unfinished backing-function)

(fact "unfinished produces a function that throws an exception"
  (backing-function 2) => (throws java.lang.Error)
  "it prints useful information about how the call was made"
  (backing-function 2 "string") => (throws java.lang.Error
                                  #"no implementation"
                                  #"\(backing-function 2 \"string\"\)"))
  
