(ns implementation.line-numbers.fim-tabular-parsing
  (:use midje.sweet
        midje.test-util))

;; Check that errors are reported with the correct line numbers. There's one check for each
;; place where line numbers are reported.

(silent-tabular
 (fact
     (cons 1 ?a) => 3))
(note-that fact-fails (failure-was-at-line 8))

