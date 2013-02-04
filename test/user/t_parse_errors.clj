(ns user.t-parse-errors
  (:use midje.sweet
        midje.test-util))

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

(silent-fact "the left-hand-side must look like a function call"
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



;; This can only be done approximately, since the right-hand-side could be a bound symbol,
;; a quoted form, etc.
;; (silent-fact "No map"
;;   (f ..new-val..) => 0
;;   (provided
;;     ...new-val.. =contains=> 3))
;; (note-that (fact-failed-with-note #"right-hand-side.*should be a map"))



;;; =====================================              Background forms
;; `background` finds parse errors of its background changers"
;; For this first case, we check each of the types of background changers
;; (fake, data-fake, and code-runner). In later background forms, we'll only
;; check one because they route through the same code.

(capturing-failure-output  ;; a bad prerequisite
 (macroexpand-1 '(background f => 2))
 (fact @fact-output => #"prerequisite must look like a function call"))


(capturing-failure-output  ;; a bad metadata prerequisite
 (macroexpand-1 '(background f =contains=> {:a 2}))
 (fact @fact-output => #"not a metaconstant"))

(prn "need a changer parse error")

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
 
  
;;; =====================================              Outside-fact against-background
;; `background` finds parse errors of its background changers"

(capturing-failure-output  ;; a bad prerequisite
 (macroexpand-1 '(against-background [f => 2] (fact 1 => 2)))
 (fact @fact-output => #"prerequisite must look like a function call"))

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




;; (fact "before gets an optional :after param"
;;   (validate-old `(before :contents (do "something") :after (do "another thing"))) =not=> validation-error-form?
;;   (validate-old `(before :contents (do "something") :around (do "another thing"))) => validation-error-form?)

;; (fact "after and around don't get extra params - length should be 3"
;;   (validate-old `(after :contents (do "something") :after (do "another thing"))) => validation-error-form?
;;   (validate-old `(around :contents (do "something") :after (do "another thing"))) => validation-error-form?)

;; (facts "against-background validation"

;;   (fact "valid, then return rest of form"
;;     (validate-old `(against-background [(before :contents (do "something")) 
;;                                     (after :checks (do "something"))]
;;                  "body")) => `([(before :contents (do "something")) 
;;                                           (after :checks (do "something"))] "body")
  
;;     (validate-old `(against-background (before :contents (do "something")) 
;;                  "body")) 
;;     => 
;;     `( (before :contents (do "something")) 
;;          "body") )
    
;;   (fact "invalid if any state-description invalid"
;;     (validate-old `(against-background [(before :contents (do "something"))
;;                                     (after :BAD (do "something"))]
;;                  "body")) => validation-error-form?
;;     (validate-old `(against-background (before :BAD (do "something"))
;;                  "body")) => validation-error-form? ) 
  
;;   (fact "invalid when the second in form is not state-descriptions and/or bckground fakes" 
;;     (validate-old `(against-background :incorrect-type-here "body")) => validation-error-form? )
  
;;   (fact "invalid when form has less than 3 elements" 
;;     (validate-old `(against-background [(before :contents (do "something"))
;;                                     (after :BAD (do "something"))])) => validation-error-form? 
;;     (validate-old `(against-background (before :contents (do "something")))) => validation-error-form? ))


;; (facts "background validation"

;;   (fact "valid, then return rest of form"
;;     (validate-old `(background (before :contents (do "something")) 
;;                            (after :checks (do "something")))) 
    
;;     => `( (before :contents (do "something")) 
;;           (after :checks (do "something")))
  
;;     (validate-old `(background (before :contents (do "something")))) 
;;     => 
;;     `( (before :contents (do "something"))))
    
;;   (fact "invalid if any state-description invalid"
;;     (validate-old `(background (before :contents (do "something"))
;;                            (after :BAD (do "something")))) => validation-error-form?
;;     (validate-old `(background (before :BAD (do "something")))) => validation-error-form? ) )

;; ;;;; Validation end-to-end facts
;; (fact "background forms require particular keys"
;;   (let [error-regexp
;;         #"second element \(:invalid-wrapping-target\) should be one of: :facts, :contents, or :checks"]
;;     (silent-against-background [(before :invalid-wrapping-target (do "something"))] "body")
;;     (silent-against-background [(before :invalid-wrapping-target (do "something"))] "body")
;;     (silent-against-background (before :invalid-wrapping-target (do "something")) "body")
;;     (silent-background (before :invalid-wrapping-target (do "something")))
    
;;     (for-each-failure (note-that parser-exploded, (fact-failed-with-note error-regexp)))))



;; (defn f [])
;; ;; Badly formatted prerequisites (outside of facts)
;; (let [error-regexp #"Badly formatted background prerequisites"]
  
;;   (silent-against-background [:not-a-state-description-or-fake] (fact nil => nil))
;;   ;; check for vectors w/ one thing that isn't a state-description or background fake
;;   (silent-against-background [(before :contents (do "something")) (f) => 5 :other-odd-stuff] (fact nil => nil))
  
;;   ;; invalid when anything doesn't look like a state-description or background fake
;;   (silent-background (before :contents (do "something"))
;;      (:not-a-state-description-or-fake))
  
;;   ;; invalid when one thing isn't a state-description or background fake
;;   (silent-background :invalid-stuff-here)
  
;;   (for-each-failure (note-that parser-exploded, (fact-failed-with-note error-regexp))))


;; ;; invalid because  missing background fakes or state descriptions (outside of facts)
;; (let [error-regexp #"You put nothing in the background"]
;;   (silent-against-background [] (fact nil => nil))
;;   (silent-background)
  
;;   (for-each-failure (note-that parser-exploded, (fact-failed-with-note error-regexp))))


;; (silent-fact "Background statements within facts participate in validation"
;;   (silent-against-background (:not-a-state-description-or-fake) (fact nil => nil))
;;   (note-that parser-exploded))

;; ;; A different error message for `against-background` vs. `background`, which
;; ;; seems gratuitous.
;; (silent-background :invalid-stuff-here)
;; (note-that (fact-failed-with-note #"Badly formatted background prerequisite"))
;; (silent-against-background :invalid-stuff-here (fact nil => nil))
;; (note-that (fact-failed-with-note #"Malformed against-background"))

;; ;; invalid if missing background fakes or state descriptions 
;; (silent-against-background (fact nil => nil))
;; (note-that (fact-failed-with-note #"You need a minimum of three elements"))
