;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checkers.simple
  (:use [midje.checkers.defining :only [as-checker checker defchecker]]
  	[midje.checkers.extended-equality :only [extended-=]]
  	[midje.checkers.util :only [named-as-call]]
  	[midje.error-handling.exceptions :only [captured-throwable?]]
    [midje.util.ecosystem :only [clojure-1-3? +M -M *M]]
    [midje.util.form-utils :only [pred-cond regex?]]
    [midje.util.backwards-compatible-utils :only [every-pred-m some-fn-m]])
  (:import [midje.error_handling.exceptions ICapturedThrowable]))

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


;; Concerning Throwables

(defmulti throws 
    (fn [& args]
      (let [arg-types (map #(pred-cond %, fn? :pred, (some-fn-m string? regex?) :msg, class? :ex) args)]
        (case arg-types
          [:pred]      :predicate
          [:msg]       :message
          [:ex]        :throwable
          [:ex :pred]  :throwable+predicate
          [:ex :msg]   :throwable+message
          [:ex :msg :pred]  :throwable+message+predicate))))

(defmethod throws :message [msg]
  (checker [^ICapturedThrowable wrapped-throwable]
    (extended-= (.getMessage ^Throwable (.throwable wrapped-throwable)) msg)))

(defmethod throws :predicate [pred?]
  (checker [^ICapturedThrowable wrapped-throwable]
    (pred? (.throwable wrapped-throwable))))

(defmethod throws :throwable [clazz]
  (checker [^ICapturedThrowable wrapped-throwable]
    (= clazz (class (.throwable wrapped-throwable)))))

(defmethod throws :throwable+predicate [clazz pred?]
  (as-checker (every-pred-m (throws clazz) (throws pred?))))

(defmethod throws :throwable+message [clazz msg]
  (as-checker (every-pred-m (throws clazz) (throws msg))))

(defmethod throws :throwable+message+predicate [clazz msg pred?]
  (as-checker (every-pred-m (throws clazz) (throws msg) (throws pred?))))