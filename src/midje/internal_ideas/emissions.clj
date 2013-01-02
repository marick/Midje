(ns ^{:doc "Emissions change global state: the output and the pass/fail record."}
  midje.internal-ideas.emissions
  (:require [midje.ideas.reporting.report :as report]
            [midje.clojure-test-facade :as ctf]
            [midje.ideas.reporting.levels :as levelly]))

(def counters (atom {}))

(defn reset-counters! []
  (reset! counters {:midje-passes 0
                    :midje-failures 0
                    :clojure-test-passes 0
                    :clojure-test-failures 0}))


(defn forget-complete-past []
  (reset-counters!)
  (ctf/zero-counters)
  (reset! levelly/last-namespace-shown nil))



(def ^{:dynamic true} emission-map
  {:pass (fn []
           (swap! counters (merge-with + {:midje-passes 1}))
           (ctf/report {:type :pass}))})
           

(defmacro make [symbol]
  `(defn ~symbol [& args#]
     (apply (~(keyword symbol) emission-map) args#)))
  
(make pass)
