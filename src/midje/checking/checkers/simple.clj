;; Note: checkers need to be exported in ../checkers.clj

(ns ^{:doc "Prepackaged functions that perform common checks."}
  midje.checking.checkers.simple
  (:use [midje.checking.checkers.defining :only [as-checker checker defchecker]]
      	[midje.checking.extended-falsehood :only [extended-false?]]
      	[midje.checking.extended-equality :only [extended-=]]
      	[midje.checking.checkers.util :only [named-as-call]]
      	[midje.error-handling.exceptions :only [captured-throwable?]]
        [midje.util.ecosystem :only [clojure-1-3? +M -M *M]]
        [midje.util.form-utils :only [defalias def-many-methods pred-cond regex?]]
        [midje.util.backwards-compatible-utils :only [every-pred-m some-fn-m]]
        [clojure.algo.monads :only [domonad set-m]])
  (:import [midje.error_handling.exceptions ICapturedThrowable]))

(defchecker truthy 
  "Returns precisely true if actual is not nil and not false."
  [actual] 
  (and (not (captured-throwable? actual))
       (boolean actual)))
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
    (named-as-call "exactly" expected
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
         (and (number? actual)
              (>= expected (-M actual delta))
              (<= expected (+M actual delta)))))
    ([expected]
       (roughly expected (abs (*M 0.001 expected))))))


;; Concerning Throwables

(letfn [(throwable-as-desired? [throwable desideratum]
           (pred-cond desideratum
                   fn?                        (desideratum throwable)
                   (some-fn-m string? regex?) (extended-= (.getMessage ^Throwable throwable) desideratum)
                   class?                     (instance? desideratum throwable)))]

  (defchecker throws
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
    [& desiderata]
    (checker [wrapped-throwable]
     (if-not (instance? ICapturedThrowable wrapped-throwable)
       false
       (let [throwable (.throwable wrapped-throwable)
             evaluations (map (partial throwable-as-desired? throwable)
                              desiderata)
             failures (filter extended-false? evaluations)]
         ;; It might be nice to return some sort of composite
         ;; failure, but I bet just returning the first one is fine,
         ;; especially since I expect people will use the class as
         ;; the first desiderata.
         (or (empty? failures) (first failures))))))
)
