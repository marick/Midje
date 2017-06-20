(ns behaviors.t-unfinished
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]))

(unfinished backing-function)

(fact "unfinished produces a function that throws an exception"
  (backing-function 2) => (throws Error))

(fact "it prints useful information about how the call was made"
  (backing-function 2 "string") => (throws Error
                                           #"no implementation"
                                           #"\(backing-function 2 \"string\"\)"))

