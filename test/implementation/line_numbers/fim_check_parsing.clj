(ns implementation.line-numbers.fim-check-parsing
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; Check that errors are reported with the correct line numbers. There's one check for each
;; place where line numbers are reported.

(unfinished f)

(silent-fact "provided is incorrectly parenthesized"
   (cons 1 ?a) => 3
   (provided (f 1)) => 3)
(note-that fact-fails (failure-was-at-line 12))

(silent-fact "Malformed previous provider"
  (f ..new-val..) ;; this is not a good check
  (provided
    (g ..new-val..) => ..new-transformed-val..))
(note-that (failure-was-in-file "fim_check_parsing.clj")
           (failure-was-at-line 17))

