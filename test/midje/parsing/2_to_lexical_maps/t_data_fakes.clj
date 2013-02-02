(ns midje.parsing.2-to-lexical-maps.t-data-fakes
  (:use [midje sweet test-util]
        midje.parsing.2-to-lexical-maps.data-fakes
        midje.util))

(silent-fact "check line number and file position of errors"
  (f) => 0
  (provided  ;; since no form on left-hand-side, this is the line number used.
    mistake =contains=> {:a 0}))
(note-that (failure-was-in-file "t_data_fakes.clj")
           (failure-was-at-line 8))

