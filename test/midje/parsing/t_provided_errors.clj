(ns midje.parsing.t-provided-errors
  (:use midje.sweet
        midje.test-util))


(unfinished f)

(silent-fact "Check line number and file position of phase 1 errors"
  (f ..new-val..)
  (provided
    (g ..new-val..) => ..new-transformed-val..))
(note-that (failure-was-in-file "t_provided_errors.clj")
           (failure-was-at-line 10))

(silent-fact "check line number and file position of phase 2 errors"
  (f) => 0
  (provided
    (deref cons) => 0))  ; for example
(note-that (failure-was-in-file "t_provided_errors.clj")
           (failure-was-at-line 18))

;; Messages

(silent-fact "Certain functions that cannot be faked are detected at parse time"
  (f) => 1
  (provided (deref cons) => 2))
(note-that (fact-failed-with-note #"You seem to have created a prerequisite for.*deref"))

(silent-fact "inlined functions cannot be faked"
  (f 3) => 0
  (provided
    (+ 3 3) => 0))
(note-that (fact-failed-with-note #"clojure.core/\+.*is inlined"))

(silent-fact "the left-hand-side must look like a function call"
  (f) => 0
  (provided
    1 => 0))
(note-that (fact-failed-with-note #"must look like a function call"))

(silent-fact "A missing right-hand-side in the preceding form"
  (f ..new-val..)
  (provided
    (g ..new-val..) => ..new-transformed-val..))
(note-that (fact-failed-with-note #"The form before the `provided`"))

(silent-fact "Misparenthesization" 
  (f ..new-val.. => 0)
  (provided
    (g ..new-val..) => ..new-transformed-val..))
(note-that (fact-failed-with-note #"The form before the `provided`"))
