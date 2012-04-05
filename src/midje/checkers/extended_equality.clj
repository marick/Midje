(ns ^{:doc "`=` extended for regular expressions, functions, etc."}
  midje.checkers.extended-equality
  (:use [clojure.core.match :only [match]]
        [midje.checkers.chatty :only [chatty-checker-falsehood?]]
        [midje.util.form-utils :only [classic-map? pairs record? regex?]]))

(defn extended-fn? [x]
  (or (fn? x)
      (= (class x) clojure.lang.MultiFn)))

(defn extended-= [actual expected]
  (letfn [(evaluate-extended-fn [] 
            (let [function-result (expected actual)]
              (if (chatty-checker-falsehood? function-result) 
                false 
                function-result)))]
    (try
      (match [actual expected]
        [(a :when chatty-checker-falsehood?) _]    actual
        [_ (e :when chatty-checker-falsehood?)]    expected
        [_ (e :when extended-fn?)]                 (evaluate-extended-fn) 
        [(a :when regex?)  (e :when regex?)]       (= (str actual) (str expected))
        [_                 (e :when regex?)]       (re-find expected actual)
        [(a :when record?) (e :when classic-map?)] (= (into {} actual) expected)
        :else                                      (= actual expected))
      (catch Throwable ex false))))

(defn extended-list-=
  "Element-by-element comparison, using extended-= for the right-hand-side values."
  [actual-args checkers]
  (every? (partial apply extended-=) (pairs actual-args checkers)))
