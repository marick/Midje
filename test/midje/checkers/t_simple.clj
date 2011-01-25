;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-simple
  (:use midje.sweet
        midje.test-util))

(facts "about truthy"
  true => truthy
  1 => truthy
  (truthy false) => false
  (truthy nil) => false)

(facts "about falsey"
  false => falsey
  nil => falsey
  (falsey true) => false
  (falsey 1) => false)

(facts "about anything"
  true => anything
  false => anything
  even? => anything)

(facts "about exactly"
  true => (exactly true)
  ( (exactly 2) 2) => truthy
  ( (exactly 1) 2) => falsey
  even? => (exactly even?))

(facts "about roughly"
  "explicit range"
  ( (roughly 2.0 1.0) 0.99) => falsey
  ( (roughly 2.0 1.0) 3.01) => falsey

  0.00 => (roughly 1.0 1.0)
  2.00 => (roughly 1.0 1.0)

  "implicit range"
  ( (roughly 1000) 998.999) => falsey
  ( (roughly 1000) 999.001) => truthy
  ( (roughly 1000) 1000.990) => truthy
  ( (roughly 1000) 1001.001) => falsey)

(defn throw-exception
  ([] (throw (NullPointerException.)))
  ([message] (throw (Error. message)))
)

(facts "about throws"
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

