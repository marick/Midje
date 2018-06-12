(ns user.fus-parse-errors
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

(unfinished f)
(defn g [n] (f n))


;; ==================                            Check expressions

(silent-fact (f 1) =>)
(note-that parse-error-found (fact-failed-with-note #"has the wrong shape"))

;; ===================                           In-fact Prerequisites

(silent-fact "Certain functions that cannot be faked are detected at parse time"
  (f) => 1
  (provided (deref cons) => 2))
(note-that parse-error-found (fact-failed-with-note #"You seem to have created a prerequisite for.*deref"))

(silent-fact "inlined functions cannot be faked"
  (f 3) => 0
  (provided
    (+ 3 3) => 0))
(note-that parse-error-found (fact-failed-with-note #"clojure.core/\+.*is inlined"))

(silent-fact "the left-hand side must look like a function call"
  (f) => 0
  (provided
    1 => 0))
(note-that parse-error-found (fact-failed-with-note #"must look like a function call"))

(silent-fact "A missing right-hand-side in the preceding form"
  (f ..new-val..)
  (provided
    (g ..new-val..) => ..new-transformed-val..))
(note-that parse-error-found (fact-failed-with-note #"The form before the `provided`"))

(silent-fact "Misparenthesization"
  (f ..new-val.. => 0)
  (provided
    (g ..new-val..) => ..new-transformed-val..))
(note-that parse-error-found (fact-failed-with-note #"The form before the `provided`"))

(silent-fact "No metaconstant"
  (f ..new-val..) => 0
  (provided
    g =contains=> {:a 3}))
(note-that parse-error-found (fact-failed-with-note #"g is not a metaconstant"))

(silent-fact "checker as right-hand side"
  (f 1) => 2
  (provided (g "2") => truthy))
(note-that parse-error-found (fact-failed-with-note #"checker on the right-hand side"))

(silent-fact "Squash cause of 'Unable to resolve symbol: provided'"
  (f 1) => 2
  (provided (g "2")) => truthy)
(note-that parse-error-found (fact-failed-with-note #"looks as though you've misparenthesized"))

(silent-fact "Cannot use a keyword in a prerequisite's call form"
  (:name ..foo..) => "name"
  (provided (:name ..foo..) => "name"))
(note-that parse-error-found (fact-failed-with-note #"prerequisite.*a var or a symbol"))


;;; =====================================              Code runners
;;; Aka before/after/around

;;; (before [:facts | :checks] <code>

(silent-fact
  (against-background (before :facts))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`before` has two forms"))

(silent-fact
  (against-background (before :facts identity :after))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`before` has two forms"))

(silent-fact
  (against-background (before :facts identity :after identity :one))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`before` has two forms"))

(silent-fact
  (against-background (before :fact identity))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"Expected the target of `before` to be :facts, :checks, or :contents"))

(silent-fact
  (against-background (before :facts identity :aft identity))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"Expected the third argument of `before` to be :after"))


;;; (after [:facts | :checks] <code>

(silent-fact
  (against-background (after :facts))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`after` takes a target and a form to run"))

(silent-fact
  (against-background (after :facts identity before))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`after` takes a target and a form to run"))

(silent-fact
  (against-background (after :fact identity))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"Expected the target of `after` to be :facts, :checks, or :contents"))


;;; (around [:facts | :checks] <code>

(silent-fact
  (against-background (around :facts))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`around` takes a target and a form to run"))

(silent-fact
  (against-background (around :facts (let [a 1] ?form) before))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"`around` takes a target and a form to run"))

(silent-fact
  (against-background (around :fact (let [a 1] ?form)))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"Expected the target of `around` to be :facts, :checks, or :contents"))

(silent-fact
  (against-background (around :facts (let [a 1] ?forms)))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"The wrapper must contain `\?form`"))



;;; =====================================              `background` forms
;; `background` finds parse errors of its background changers"
;; For this first case, we check each of the types of background changers
;; (fake, data-fake, and code-runner). In later background forms, we'll
;; check fewer because they route through the same code.

(capturing-failure-output  ;; a bad prerequisite
 (macroexpand-1 '(background f => 2))
 (fact @fact-output => #"prerequisite must look like a function call"))


(capturing-failure-output  ;; a bad metadata prerequisite
 (macroexpand-1 '(background f =contains=> {:a 2}))
 (fact @fact-output => #"not a metaconstant"))

(capturing-failure-output  ;; a bad code runner
 (macroexpand-1 '(background (before :facts)))
 (fact @fact-output => #"`before` has two forms"))


(capturing-failure-output ;; multiple failures reports only the first
 (macroexpand-1 '(background (f 1) => 1
                           f => 2
                           f =contains=> {:a 2}))
 (fact
   @fact-output => #"must look like a function call"
   @fact-output =not=> #"not a metaconstant"))

(capturing-failure-output ;; works with optional let-style vector
 (macroexpand-1 '(background [(f 1) => 1, f => 2]))
 (fact
   @fact-output => #"must look like a function call"))

(capturing-failure-output
 (macroexpand-1 '(background (f 1) => 1, "1"))
 (fact
   @fact-output => #"\"1\" does not look like"))

(capturing-failure-output
 (macroexpand-1 '(background ((f 1) => 1)))
 (fact
   @fact-output => #"\(\(f 1\) => 1\) does not look like"))

(capturing-failure-output
 (macroexpand-1 '(background (first :facts identity)))
 (fact
   @fact-output => #"\(first :facts identity\) does not look like"))


(fact ;; prerequisite forms don't object to prerequisite arrows
  (prerequisite (rand) =streams=> [1 2 3])
  1 => 1)

;;; =====================================              Outside-fact against-background
;; `background` finds parse errors of its background changers"

(capturing-failure-output  ;; a bad prerequisite
 (macroexpand-1 '(against-background [f => 2] (fact 1 => 2)))
 (fact @fact-output => #"prerequisite must look like a function call"))

(capturing-failure-output
 (macroexpand-1 '(against-background [(f 1) => 1, [1 2 3]] (fact 1 => 2)))
 (fact
   @fact-output => #"\[1 2 3\] does not look like"))


(capturing-failure-output
 (macroexpand-1 '(against-background [(f 1) => 1, (first thing)] (fact 1 => 2)))
 (fact
   @fact-output => #"\(first thing\) does not look like"))

(silent-fact "It also complains if there are no facts nested inside it"
  (against-background [(f 1) => 1]
    (inc (f 1)) => 2))
(note-that parse-error-found (fact-failed-with-note #"against-background.*only affect nested facts"))

(fact "Nested future-facts do not trigger the complaint"
  (macroexpand-1 '(against-background [(f 1) => 1] (future-fact (+ 1 1) => 3))))



;; It would be nice if it didn't complain about outer-level against-backgrounds
;; with nothing inside them, but that's more trouble than it's worth.

(capturing-failure-output
 (macroexpand-1 '(against-background [(f 1) => 1]
                   ;; TBD
                   ))
 (fact
   @fact-output => #"against-background.*only affect nested facts"))

;;; The same mechanism that would tell the code *not* to produce an
;;; error in the above case could probably be used to produce an
;;; error in the following case (without producing errors for
;;; wrapping against-backgrounds outside any fact).
(future-fact "a non-wrapping form that is too deeply nested"
  (let [something 'some-value]
    (f [3]) => 3
    (against-background (g 3) => 3))) ;; incorrectly within the let body



(fact "it is not fooled by nested against-backgrounds "
  (against-background [(f 1) => 1]
    (against-background [(f 2) => 2]
      (fact
        (+ (f 1) (f 2)) => 3)))

  (fact "... which most likely happen by mixing the two forms"
    (against-background [(f 2) => 3]
      (fact
        (against-background [(f 3) => 4])
        (+ (f 2) (f 3)) => 7))))


;;; =====================================              Inside-fact against-background
;; `background` finds parse errors of its background changers"

(silent-fact
  (against-background (+ 1 2) => 2)
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"clojure.core/\+.*is inlined"))

(silent-fact "[changer] variant"
  1 => 2
  (against-background [(g 1) => 2, (+ 1 2) => 2]))
(note-that parse-error-found (fact-failed-with-note #"clojure.core/\+.*is inlined"))

(silent-fact "background variant"
  (background (g 1) => 2, (+ 1 2) => 2)
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"clojure.core/\+.*is inlined"))


(silent-fact "both variants"
  (background [(g 1) => 2, (+ 1 2) => 2])
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"clojure.core/\+.*is inlined"))

(silent-fact "weird-looking values"
  (against-background [1 2 3])
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"1 does not look like"))

(silent-fact "nothing like a code runner"
  (against-background (cons 1 [2]))
  1 => 2)
(note-that parse-error-found (fact-failed-with-note #"\(cons 1 \[2\]\) does not look like"))

