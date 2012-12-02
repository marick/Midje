(ns as-documentation.t-prerequisites
  (:use midje.sweet
        midje.util
        midje.test-util)
  (:require [midje.config :as config]))

;; One development path is to work top-down and use the `provided`
;; clause to substitute prerequisite values.

(unfinished lower-function)

(defn top-function [n]
  (+ (lower-function n) (lower-function (inc n))))

(fact
  (top-function 5) => 55
  (provided
    (lower-function 5) => 50
    (lower-function 6) =>  5))

;; If you leave off a prerequisite, you get a helpful failure:

(capturing-output
 (fact
   (top-function 5) => 55
   (provided
     ;; (lower-function 5) => 50    ; omit this one.
     (lower-function 6) =>  5))
 ;; So...
 (fact 
   @test-output => #"You never said lower-function would be needed with these arguments:"
   @test-output => #"\(5\)"))

   
;; You also get a helpful failure for an unused prerequisite:       

(capturing-output
 (fact
   (top-function 5) => 5555
   (provided
     (lower-function 3) => 5000    ; unused
     (lower-function 4) =>  500    ; unused
     (lower-function 5) =>   50    ; omit this one.
     (lower-function 6) =>    5))
 ;; So...
 (fact
   @test-output => #"These calls were not made the right number of times:"
   @test-output => #"\(lower-function 3\) \[expected at least once, actually never called\]"
   @test-output => #"\(lower-function 4\) \[expected at least once, actually never called\]"
   ;; You also get a message about the failure:
   @test-output => #"Expected: 5555"
   @test-output => #"Actual: 55"))


;; By default, prerequisites can be called one or more times. The
;; :times modifier lets you change that. Here's how you can insist
;; a prerequisite be called twice:

(capturing-output
 (fact
   (top-function 5) => 55
   (provided
     (lower-function 5) => 50 :times 2
     (lower-function 6) =>  5))
 ;; So...
 (fact
   @test-output => #"\(lower-function 5\) \[expected :times 2, actually called one time\]"))

; You can also give a range of allowed values. Here's how you'd ask
; for a function to be called one or two times:

(after-silently
 (fact
   (top-function 5) => 55
   (provided
     (lower-function 5) => 50 :times [1 2]
     (lower-function 6) =>  5))
 (fact
   @reported => (just pass)))
 
;; You can also use a lazy sequence:

(after-silently
 (fact
   (top-function 5) => 55
   (provided
     (lower-function 5) => 50 :times (range 3 33)
     (lower-function 6) =>  5))
 (fact
   @reported => (just wrong-call-count
                      pass))) ;; It does give the right answer, for the wrong reason.


;; Here is the idiom for "this call is optional" (zero or more times)

(after-silently
 (fact
   (top-function 5) => 55
   (provided
     (lower-function 0) => 88 :times (range)
     (lower-function 5) => 50 
     (lower-function 6) =>  5))
 (fact
   @reported => (just pass)))


;;;                     Default prerequisites

;; Sometimes the prerequisite function already exists. What should
;; happen if there's no prerequisite for a particular argument list?
;; Should it default to the existing function or not? The Midje users
;; who care prefer that such a case be an error:

(defn I-am-I-cried [n] n)

(defn using-function [n]
  (+ (I-am-I-cried n) (I-am-I-cried (inc n))))

(after-silently
 (fact
   (using-function 4) => (+ 80 4)
   (provided
     (I-am-I-cried 5) => 80))
 (fact
   @reported => (contains no-matching-prerequisite bad-result)))

;; However, it's also possible to ask that unmatched calls default to
;; the real values. The config/with-augmented-config simulates the
;; loading of a .midje.clj file.

(config/with-augmented-config {:partial-prerequisites true}
  (after-silently
   (fact
     (using-function 4) => (+ 80 4)
     (provided
       (I-am-I-cried 5) => 80))
   (fact
     @reported => (just pass))))

