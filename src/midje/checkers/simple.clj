;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checkers.simple
  (:use [midje.checkers.defining :only [checker defchecker]]
  	[midje.checkers.extended-equality :only [extended-=]]
  	[midje.checkers.util :only [named-as-call]]
  	[midje.error-handling.exceptions :only [captured-throwable?]]
    [midje.util.ecosystem :only [clojure-1-3? +M -M *M]])
  (import [midje.error_handling.exceptions ICapturedThrowable]))

(defchecker truthy 
  "Returns precisely true if actual is not nil and not false."
  [actual] 
  (and (not (captured-throwable? actual))
       (not (not actual))))
(def TRUTHY truthy)

(defchecker falsey 
  "Returns precisely true if actual is nil or false."
  [actual] 
  (not actual))
(def FALSEY falsey)

(defchecker anything
  "Accepts any value"
  [actual]
  (not (captured-throwable? actual)))
(def irrelevant anything)

(defchecker exactly
  "Checks for equality. Use to avoid default handling of functions."
  [expected]
    (named-as-call 'exactly expected
                   (checker [actual] (= expected actual))))

(defn- abs [n]
  (if (pos? n)
    n
    (-M n))) ;; -M not strictly necessary, but...

(defchecker roughly
  "With two arguments, accepts a value within delta of the
   expected value. With one argument, the delta is 1/1000th
   of the expected value."
  ([expected delta]
     (checker [actual]
       (and (>= expected (-M actual delta))
            (<= expected (+M actual delta)))))
  ([expected]
     (roughly expected (abs (*M 0.001 expected)))))

;;Concerning Throwables

(defchecker throws
  "Checks that Throwable of named class was thrown and, optionally, that
   the message is as desired. Takes an optional predicate the
   exception must satisfy."
  ([expected-ex-class-or-pred]
      (if (fn? expected-ex-class-or-pred)
        (checker [^ICapturedThrowable wrapped-throwable]
          (expected-ex-class-or-pred (.throwable wrapped-throwable)))    
        (throws expected-ex-class-or-pred (constantly true))))

  ([expected-ex-class msg-or-pred]
       (if (fn? msg-or-pred)
         (checker [^ICapturedThrowable wrapped-throwable]
           (and (= expected-ex-class (class (.throwable wrapped-throwable)))
                (msg-or-pred (.throwable wrapped-throwable))))
         (throws expected-ex-class msg-or-pred (constantly true)))) 

  ([expected-ex-class msg pred]
     (checker [^ICapturedThrowable wrapped-throwable]
         (and (= expected-ex-class (class (.throwable wrapped-throwable)))
              (pred (.throwable wrapped-throwable)))
              (extended-= (.getMessage ^Throwable (.throwable wrapped-throwable))
                          msg))))