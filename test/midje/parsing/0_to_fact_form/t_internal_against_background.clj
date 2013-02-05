(ns ^{:doc "A fact's internal against-background ends up wrapping it"}
  midje.parsing.0-to-fact-form.t-internal-against-background
  (:use midje.sweet)
  (:require [midje.config :as config]
            [midje.data.compendium :as compendium]
            [midje.data.fact :as fact]))

(defn f [] nil)

(fact "two"
  (against-background (f) => 2)
  (f) => 2)

(fact :check-only-at-load-time
  (fact/description (compendium/last-fact-checked<>)) => "two"
  (fact/line (compendium/last-fact-checked<>)) => 10)
