;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checkers.simple
  (:use [midje.checkers util extended-equality]))

(defn truthy 
  "Returns precisely true if actual is not nil and not false."
  {:midje/checker true}
  [actual] 
  (and (not (captured-exception? actual))
       (not (not actual))))

(defn falsey 
  "Returns precisely true if actual is nil or false."
  {:midje/checker true}
  [actual] 
  (not actual))

(defn anything
  "Accepts any value"
  {:midje/checker true}
  [actual]
  (not (captured-exception? actual)))
(def irrelevant anything)

(defn exactly
  "Checks for equality. Use to avoid default handling of functions."
  {:midje/checker true}
  [expected]
    (named 'exactly expected
           (fn [actual] (= expected actual))))

(defn roughly
  "With two arguments, accepts a value within delta of the
   expected value. With one argument, the delta is 1/1000th
   of the expected value."
  {:midje/checker true}
  ([expected delta]
     (fn [actual]
       (and (>= expected (- actual delta))
            (<= expected (+ actual delta)))))
  ([expected]
     (roughly expected (* 0.001 expected))))

;;Concerning Throwables

(defn throws
  "Checks that Throwable of named class was thrown and, optionally, that
   the message is as desired."
  {:midje/checker true}
  ([expected-exception-class]
     (fn [wrapped-throwable] (throwable-with-class? wrapped-throwable expected-exception-class)))
  ([expected-exception-class message]
     (fn [wrapped-throwable]
       (and (throwable-with-class? wrapped-throwable expected-exception-class)
            (extended-= (.getMessage (wrapped-throwable captured-exception-key))
                        message)))))

