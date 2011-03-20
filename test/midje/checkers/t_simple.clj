;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-simple
  (:use midje.sweet
        [midje.checkers.defining :only [checker?]]
        midje.test-util))

(facts "about truthy"
  #'truthy => checker?
  truthy => checker?
  true => truthy
  1 => truthy
  (truthy false) => false
  (truthy nil) => false)

(facts "about falsey"
  #'falsey => checker?
  falsey => checker?
  false => falsey
  nil => falsey
  (falsey true) => false
  (falsey 1) => false)

(facts "about anything"
  #'anything => checker?
  anything => checker?
  true => anything
  false => anything
  even? => anything
  "irrelevant is a synonym"
  1 => irrelevant)

(facts "about exactly"
  #'exactly => checker?
  exactly => checker?
  (exactly odd?) =not=> checker? ;; It represents the function itself.
  true => (exactly true)
  ( (exactly 2) 2) => truthy
  ( (exactly 1) 2) => falsey
  even? => (exactly even?))

(facts "about roughly"
  "a checker that produces checkers"
  #'roughly => checker?
  roughly => checker?
  (roughly 3) => checker?
  (roughly 3 1) => checker?

  "explicit range"
  0.99 =not=> (roughly 2.0 1.0)
  3.01 =not=> (roughly 2.0 1.0)

  0.00 => (roughly 1.0 1.0)
  2.00 => (roughly 1.0 1.0)

  "implicit range"
  ( (roughly 1000) 998.999) => falsey
  ( (roughly 1000) 999.001) => truthy
  ( (roughly 1000) 1000.990) => truthy
  ( (roughly 1000) 1001.001) => falsey)

  998.999 => (roughly 1000)
  999.001 => (roughly 1000)
  1000.990 => (roughly 1000)
  1001.001 => (roughly 1000)

(defn throw-exception
  ([] (throw (NullPointerException.)))
  ([message] (throw (Error. message)))
)

(facts "about throws"
  #'throws => checker?
  throws => checker?
  (throws NullPointerException) => checker?
  (throws NullPointerException "hi") => checker?
  
  (throw-exception) => (throws NullPointerException)
  (throw-exception "hi") => (throws Error "hi")
  (throw-exception "hi") => (throws Error #"h."))

(after-silently 
 (fact 
   (throw-exception "throws Error") => (throws NullPointerException)
   (throw-exception "throws Error") => (throws Error "bye"))
 (fact 
   @reported => (two-of checker-fails)))

;; Unexpected exceptions
(after-silently
 (facts
   (throw-exception "throws Error") => anything
   (throw-exception "throws Error") => falsey
   (throw-exception "throws Error") => truthy)
 (fact
   @reported => (three-of checker-fails)))

