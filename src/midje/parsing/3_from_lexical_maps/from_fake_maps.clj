(ns ^{:doc "The semi-sweet representation of provided forms."}
  midje.parsing.3-from-lexical-maps.from-fake-maps
  (:use midje.clojure.core
        midje.checking.core
        [midje.checkers :only [exactly]]
        [midje.checking.checkers.defining :only [checker?]]
        [midje.parsing.arrow-symbols])
  (:require [midje.util.exceptions :as exceptions]))

(defn mkfn:arg-matcher
  "Based on an expected value, generates a function that returns
  true if the actual value matches it."
  [expected]
  (if (and (extended-fn? expected)
           (not (checker? expected)))
    (fn [actual] (extended-= actual (exactly expected)))
    (fn [actual] (extended-= actual expected))))

(defn mkfn:arg-matchers-with-arity
  "Generates a function that returns true if all the matchers return true for the actual args its passed."
  [matchers]
  (fn [actual-args]
    (let [arg-matchers (map mkfn:arg-matcher matchers)]
       (and (= (count actual-args) (count arg-matchers))
            (extended-list-= actual-args arg-matchers)))))

(defn mkfn:arg-matchers-without-arity [matchers]
  "Generates a function that returns true if all the matchers return true but it ignores arity matching."
  (fn [actual-args]
    (let [arg-matchers (map mkfn:arg-matcher matchers)]
       (extended-list-= actual-args arg-matchers))))

(defn mkfn:arg-matchers [arg-matchers]
  (if (= (str (first arg-matchers)) "&")
    `(mkfn:arg-matchers-without-arity ~(vec (rest arg-matchers)))
    `(mkfn:arg-matchers-with-arity    ~(vec arg-matchers))))

(defmulti mkfn:result-supplier (fn [arrow & _] arrow))

(defmethod mkfn:result-supplier => [_arrow_ result] (constantly result))

(defmethod mkfn:result-supplier =streams=> [_arrow_ result-stream]
  (let [the-stream (atom result-stream)]
    (fn []
      (when (empty? @the-stream)
        (throw (exceptions/user-error "Your =stream=> ran out of values.")))
      (let [current-result (first @the-stream)]
        (swap! the-stream rest)
        current-result))))

(defmethod mkfn:result-supplier =throws=> [_arrow_ throwable]
  (fn []
    (when-not (instance? Throwable throwable) 
      (throw (exceptions/user-error "Right side of =throws=> should extend Throwable.")))
    (throw throwable)))

(defmethod mkfn:result-supplier :default [arrow result-stream]
  (throw (exceptions/user-error "It's likely you misparenthesized your metaconstant prerequisite,"
                     "or that you forgot to use an arrow in your provided form.")))
