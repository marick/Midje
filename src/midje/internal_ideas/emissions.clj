(ns ^{:doc "Emissions change global state: the output and the pass/fail record."}
  midje.internal-ideas.emissions
  (:require [midje.ideas.reporting.report :as report]
            [midje.clojure-test-facade :as ctf]
            [midje.ideas.reporting.levels :as levelly]
            [midje.internal-ideas.state :as state]))

(defn forget-complete-past []
  (state/reset-output-counters!)
  (ctf/zero-counters)
  (reset! levelly/last-namespace-shown nil))

(alter-var-root #'state/emission-functions 
                (constantly {:pass (fn []
                                     (state/output-counters:inc:midje-passes!)
                                     (ctf/report {:type :pass}))}))

(defmacro make [symbol]
  `(defn ~symbol [& args#]
     (apply (~(keyword symbol) state/emission-functions) args#)))
       
  
(make pass)
