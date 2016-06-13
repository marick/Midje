(ns midje.parsing.2-to-lexical-maps.t-data-fakes
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.parsing.2-to-lexical-maps.data-fakes :refer :all]
            [midje.util :refer :all]))

(silent-fact "check line number and file position of errors"
  (f) => 0
  (provided  ;; since no form on left-hand-side, this is the line number used.
    mistake =contains=> {:a 0}))
(note-that (failure-was-in-file "t_data_fakes.clj")
           (failure-was-at-line 10))

