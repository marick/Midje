(ns midje.parsing.t-provided-errors
  (:use midje.sweet
        midje.test-util))


(fact "A missing right-hand-side in the preceding form"
  (macroexpand '(facts 
                  (f ..new-val..)
                  (provided
                    (g ..new-val..) => ..new-transformed-val..)))
  => (throws #"The form before the `provided` is not a check"
             #"\(f ..new-val..\)"))

(fact "Misparenthesization" 
  (macroexpand '(facts
                  (f ..new-val.. => 0)
                  (provided
                    (g ..new-val..) => ..new-transformed-val..)))
  => (throws #"The form before the `provided` is not a check"
             #"..new-val.. => 0"))
