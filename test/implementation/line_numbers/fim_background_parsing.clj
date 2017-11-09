(ns implementation.line-numbers.fim-background-parsing
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; Check that errors are reported with the correct line numbers. There's one
;; check for each place where line numbers are reported.

(unfinished f)

(silent-fact
  (+ 1 1) => 2
  (+ 1 3) => 4
  (against-background (cons 1 2)))
(note-that fact-fails (failure-was-at-line 10)) ; at fact: not so good, but OK.

(silent-against-background [(cons 1 2)]
  (fact (+ 1 1) => 2))
(note-that fact-fails (failure-was-at-line 16))

(silent-fact
  (against-background (before :facts)))
(note-that fact-fails (failure-was-at-line 21))

(silent-fact
  (against-background [(f 0 0) => 1])
  (f 1 1) => irrelevant)
(note-that fact-fails (failure-was-at-line 25))
