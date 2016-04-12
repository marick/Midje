(ns midje.parsing.2-to-lexical-maps.t-fakes
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.parsing.2-to-lexical-maps.fakes :refer :all]
            [midje.util :refer :all]))

(silent-fact "check line number and file position of t_fakes errors"
  (f) => 0
  (provided
    (deref cons) => 0))  ; for example
(note-that (failure-was-in-file "t_fakes.clj")
           (failure-was-at-line 11))

