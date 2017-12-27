(ns ^{:doc "Generating functions that work on fake maps."}
  midje.parsing.3-from-lexical-maps.from-fake-maps
  (:require [midje.checkers :as checkers]
            [midje.checking.core :refer :all]
            [midje.checking.checkers.defining :refer [checker?]]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.util.exceptions :as exceptions]
            [such.types :as types]))

(defn- mkfn:arg-matcher
  "Based on an expected value, generates a function that returns
  true if the actual value matches it."
  [expected]
  (if (and (types/extended-fn? expected)
           (not (checker? expected)))
    (fn [actual] (extended-= actual (checkers/exactly expected)))
    (fn [actual] (extended-= actual expected))))

(defn mkfn:arglist-matcher-fixed-arity
  "Generates a function that returns true if all the matchers return true for the actual args it's passed."
  [& arg-descriptions]
  (fn [actual-args]
    (extended-list-= actual-args
                     (map mkfn:arg-matcher arg-descriptions))))

(defn mkfn:arglist-matcher-allowing-optional-args
  "Generates a function that attempts to match required and optional args."
  [& arg-descriptions]
  (let [required-count (- (count arg-descriptions) 2)
        required-arglist-descriptions (take required-count arg-descriptions)
        rest-arg-description (last arg-descriptions)
        required-arg-matchers (map mkfn:arg-matcher required-arglist-descriptions)
        rest-arg-matcher (mkfn:arg-matcher rest-arg-description)]
    (fn [actual-args]
      (let [[required-actual rest-actual] (split-at required-count actual-args)]
        (and (extended-list-= required-actual required-arg-matchers)
             (extended-= rest-actual rest-arg-matcher))))))

(defmulti mkfn:result-supplier (fn [arrow & _] arrow))

(defmethod mkfn:result-supplier => [_arrow_ result] result)

(defmethod mkfn:result-supplier =streams=> [_arrow_ result-stream]
  (let [the-stream (atom (result-stream))]
    (fn []
      (when (empty? @the-stream)
        (throw (exceptions/user-error "Your =stream=> ran out of values.")))
      (let [current-result (first @the-stream)]
        (swap! the-stream rest)
        current-result))))

(defmethod mkfn:result-supplier =throws=> [_arrow_ wrapped-throwable]
  (fn []
    (let [throwable (wrapped-throwable)]
      (when-not (instance? Throwable throwable)
        (throw (exceptions/user-error "Right side of =throws=> should extend Throwable.")))
      (throw throwable))))

(defmethod mkfn:result-supplier :default [arrow result-stream]
  (throw (exceptions/user-error "It's likely you misparenthesized your metaconstant prerequisite,"
                     "or that you forgot to use an arrow in your provided form.")))
