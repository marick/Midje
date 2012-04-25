(ns ^{:doc "`=` extended for regular expressions, functions, etc."}
  midje.checkers.extended-equality
  (:use [midje.checkers.extended-falsehood :only [data-laden-falsehood?]]
        [midje.util.form-utils :only [classic-map? extended-fn? pairs record? regex?]]))

(defn extended-= [actual expected]
  (letfn [(evaluate-extended-fn [] 
            (let [function-result (expected actual)]
              (if (data-laden-falsehood? function-result) 
                false 
                function-result)))]
    (try
      (cond (data-laden-falsehood? actual)
            actual

            (data-laden-falsehood? expected)
            expected

            (extended-fn? expected)
            (evaluate-extended-fn)

            (and (regex? actual)
                 (regex? expected))
            (= (str actual) (str expected))

            (regex? expected)
            (re-find expected actual)

            (and (record? actual)
                 (classic-map? expected))
            (= (into {} actual) expected)

            :else
            (= actual expected))
      (catch Throwable ex false))))

(defn extended-list-=
  "Element-by-element comparison, using extended-= for the right-hand-side values."
  [actual-args checkers]
  (every? (partial apply extended-=) (pairs actual-args checkers)))
