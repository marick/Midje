(ns user.t-parse-exceptions
  (:use midje.sweet
        midje.test-util))


;;; In general, it would be nice if these could be moved over to t-parse-errors.

(silent-fact "malformed check"
  (fact (f 1) =>))
(note-that parser-threw-exception)
