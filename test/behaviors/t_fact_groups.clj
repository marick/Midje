(ns behaviors.t-fact-groups
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.repl :as repl]
            [midje.config :as config]))

(repl/forget-facts *ns*)
(fact-group :outer (fact-group :inner (fact :f 1 => 1)))

(let [metadata (meta (first (repl/fetch-facts *ns*)))]
  (fact "#275: metadata on nested facts accumulates"
    ((juxt :outer :inner :f :expect-nil) metadata) =future=> [true true true nil]))

;; Some other cases, for completeness

(repl/forget-facts *ns*)
(fact-group :outer (fact-group :inner (fact 1 => 1)))

(let [metadata (meta (first (repl/fetch-facts *ns*)))]
  (fact "#275: without :f"
    ((juxt :outer :inner) metadata) =future=> [true true]))


(repl/forget-facts *ns*)
(fact-group :outer (fact-group (fact :f 1 => 1)))

(let [metadata (meta (first (repl/fetch-facts *ns*)))]
  (fact "without :inner"
    ((juxt :outer :inner :f) metadata) => [true nil true]))


(repl/forget-facts *ns*)
(fact-group (fact-group :inner (fact :f 1 => 1)))

(let [metadata (meta (first (repl/fetch-facts *ns*)))]
  (fact "#275: without :outer"
    ((juxt :outer :inner :f) metadata) =future=> [nil true true]))


(repl/forget-facts *ns*)
(fact-group (fact-group (fact :f 1 => 1)))

(let [metadata (meta (first (repl/fetch-facts *ns*)))]
  (fact "without inner and outer"
    ((juxt :outer :inner :f) metadata) => [nil nil true]))



