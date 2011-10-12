;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checkers.simple
  (:use [midje.checkers.defining :only [checker defchecker]]
  	[midje.checkers.extended-equality :only [extended-=]]
  	[midje.checkers.util :only [captured-exception? 
  	                            captured-exception-key 
  	                            named-as-call
  	                            throwable-with-class?]]
        [midje.util.ecosystem :only [clojure-1-3? +M -M *M]]))

(defchecker truthy 
  "Returns precisely true if actual is not nil and not false."
  [actual] 
  (and (not (captured-exception? actual))
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
  (not (captured-exception? actual)))
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
   the message is as desired."
  ([expected-exception-class]
     (checker [wrapped-throwable] (throwable-with-class? wrapped-throwable expected-exception-class)))
  ([expected-exception-class message]
     (checker [wrapped-throwable]
       (and (throwable-with-class? wrapped-throwable expected-exception-class)
            (extended-= (.getMessage (wrapped-throwable captured-exception-key))
                        message)))))
