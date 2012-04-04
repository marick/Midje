(ns midje.internal-ideas.t-unfinished
  (:use [midje sweet test-util]))

(unfinished backing-function)

(fact "unfinished produces a function that throws an exception"
  (backing-function 2) => (throws Error)
  "it prints useful information about how the call was made"
  (backing-function 2 "string") => (throws Error
                                  #"no implementation"
                                  #"\(backing-function 2 \"string\"\)"))
  
