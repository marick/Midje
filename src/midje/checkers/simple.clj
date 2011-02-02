;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checkers.simple
  (:use [midje.checkers util extended-equality defining]))

(defchecker truthy 
  "Returns precisely true if actual is not nil and not false."
  [actual] 
  (and (not (captured-exception? actual))
       (not (not actual))))

(defchecker falsey 
  "Returns precisely true if actual is nil or false."
  [actual] 
  (not actual))

(defchecker anything
  "Accepts any value"
  [actual]
  (not (captured-exception? actual)))
(def irrelevant anything)

(defchecker exactly
  "Checks for equality. Use to avoid default handling of functions."
  [expected]
    (named 'exactly expected
           (checker [actual] (= expected actual))))

;; TODO: needs to use defchecker
(defn roughly
  "With two arguments, accepts a value within delta of the
   expected value. With one argument, the delta is 1/1000th
   of the expected value."
  ([expected delta]
     (checker [actual]
       (and (>= expected (- actual delta))
            (<= expected (+ actual delta)))))
  ([expected]
     (roughly expected (* 0.001 expected))))

;;Concerning Throwables

;; TODO: needs to use defchecker
(defn throws
  "Checks that Throwable of named class was thrown and, optionally, that
   the message is as desired."
  ([expected-exception-class]
     (checker [wrapped-throwable] (throwable-with-class? wrapped-throwable expected-exception-class)))
  ([expected-exception-class message]
     (checker [wrapped-throwable]
       (and (throwable-with-class? wrapped-throwable expected-exception-class)
            (extended-= (.getMessage (wrapped-throwable captured-exception-key))
                        message)))))

