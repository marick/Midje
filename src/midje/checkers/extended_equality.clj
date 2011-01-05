;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.extended-equality
  (:use [midje.checkers.chatty :only [chatty-checker-falsehood?]]
        [midje.util.form-utils :only [regex? pairs]]))

(defn extended-fn? [x]
  (or (fn? x)
      (= (class x) clojure.lang.MultiFn)))

(defn extended-= [actual expected]
  (try
    (cond (chatty-checker-falsehood? actual)
          actual

          (chatty-checker-falsehood? expected)
          expected
          
          (extended-fn? expected)
          (let [function-result (expected actual)]
            (if (chatty-checker-falsehood? function-result) false function-result))
        
          (regex? expected)
          (if (regex? actual)
            (= (.toString actual) (.toString expected))
            (re-find expected actual))

          :else
          (= actual expected))
    (catch Exception ex false)))

(defn extended-list-= [actual-args checkers]
  "Element-by-element comparison, using extended-= for the right-hand-side values."
  (every? (fn [ [actual checker] ] (extended-= actual checker))
   	  (pairs actual-args checkers)))

