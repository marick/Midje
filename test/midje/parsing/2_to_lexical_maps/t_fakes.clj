(ns midje.parsing.2-to-lexical-maps.t-fakes
  (:use [midje sweet test-util]
        midje.parsing.2-to-lexical-maps.fakes
        midje.util))

(silent-fact "check line number and file position of t_fakes errors"
  (f) => 0
  (provided
    (deref cons) => 0))  ; for example
(note-that (failure-was-in-file "t_fakes.clj")
           (failure-was-at-line 9))

