;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns ^{:doc "Prepackaged functions that perform common checks."}
  midje.checkers.simple
  (:use [midje.checkers.defining :only [as-checker checker defchecker]]
  	[midje.checkers.extended-equality :only [extended-=]]
  	[midje.checkers.util :only [named-as-call]]
  	[midje.error-handling.exceptions :only [captured-throwable?]]
    [midje.util.ecosystem :only [clojure-1-3? +M -M *M]]
    [midje.util.form-utils :only [defalias def-many-methods pred-cond regex?]]
    [midje.util.backwards-compatible-utils :only [every-pred-m some-fn-m]])
  (:import [midje.error_handling.exceptions ICapturedThrowable]))

(defchecker truthy 
  "Returns precisely true if actual is not nil and not false."
  [actual] 
  (and (not (captured-throwable? actual))
       (not (not actual))))
(defalias TRUTHY truthy)

(defchecker falsey 
  "Returns precisely true if actual is nil or false."
  [actual] 
  (not actual))
(defalias FALSEY falsey)

(defchecker anything
  "Accepts any value."
  [actual]
  (not (captured-throwable? actual)))
(defalias irrelevant anything)

(defchecker exactly
  "Checks for equality. Use to avoid default handling of functions."
  [expected]
    (named-as-call 'exactly expected
                   (checker [actual] (= expected actual))))

(letfn [(abs [n]
          (if (pos? n)
            n
            (-M n)))] ;; -M not strictly necessary, but...

  (defchecker roughly
    "With two arguments, accepts a value within delta of the
     expected value. With one argument, the delta is 1/1000th
     of the expected value."
    ([expected delta]
       (checker [actual]
         (and (>= expected (-M actual delta))
              (<= expected (+M actual delta)))))
    ([expected]
       (roughly expected (abs (*M 0.001 expected))))))


;; Concerning Throwables

(defmulti throws
  "Checks for a thrown Throwable.

   The most common cases are:
       (fact (foo) => (throws IOException)
       (fact (foo) => (throws IOException #\"No such file\")

   `throws` takes three kinds of arguments: 
   * A class argument requires that the Throwable be of that class.
   * A string or regular expression requires that the `message` of the Throwable
     match the argument.
   * A function argument requires that the function, when applied to the Throwable,
     return a truthy value.

   Arguments can be in any order. Except for a class argument, they can be repeated.
   So, for example, you can write this:
       (fact (foo) => (throws #\"one part\" #\"another part\"))"
  (fn [& args]
    (set (for [arg args]
           (pred-cond arg
                      fn?                        :predicate
                      (some-fn-m string? regex?) :message
                      class?                     :throwable )))))

(defmethod throws #{:message } [& expected-msgs]
  (checker [^ICapturedThrowable wrapped-throwable]
    (let [actual-msg (.getMessage ^Throwable (.throwable wrapped-throwable))]
      (every? (partial extended-= actual-msg) expected-msgs))))

(defmethod throws #{:predicate} [& preds]
  (checker [^ICapturedThrowable wrapped-throwable]
    ((apply every-pred-m preds) (.throwable wrapped-throwable))))

(defmethod throws #{:throwable} [clazz]
  (checker [^ICapturedThrowable wrapped-throwable]
    (= clazz (class (.throwable wrapped-throwable)))))

(def-many-methods throws [#{:throwable :predicate}, #{:message :predicate },
                          #{:throwable :message}, #{:throwable :message :predicate}] [& args]
  (as-checker (apply every-pred-m (map throws args))))
