(ns ^{:doc "`=` extended for regular expressions, functions, etc."}
  midje.checkers.extended-equality
  (:use [midje.checkers.extended-falsehood :only [as-data-laden-falsehood data-laden-falsehood?]]
        [midje.util.form-utils :only [classic-map? extended-fn? pairs record? regex?]]))


(defn evaluate-checking-function
  "Returns a sequence. The first value is either truthy or falsey.
   If falsey, the second value is a map with
   any additional information. (It may be empty.) If the
   result is an exception, the second value contains it under
   the :thrown key."
  [function actual]
  (try
    (let [function-result (function actual)]
      (if (data-laden-falsehood? function-result)
        [false function-result]
        [function-result {}]))
  (catch Throwable ex
    [false {:thrown ex}])))

(defn extended-= [actual expected]
  (try  
    (cond
     (data-laden-falsehood? actual)      actual
     (data-laden-falsehood? expected)    expected
     (extended-fn? expected)             (first (evaluate-checking-function expected actual))
     (every? regex? [actual expected])   (= (str actual) (str expected))
     (regex? expected)                   (re-find expected actual)
     (and (record? actual) (classic-map? expected))   (= (into {} actual) expected)
     :else                               (= actual expected))
    (catch Throwable ex false)))

(defn extended-list-=
  "Element-by-element comparison, using extended-= for the right-hand-side values."
  [actual-args checkers]
  (every? (partial apply extended-=) (pairs actual-args checkers)))
