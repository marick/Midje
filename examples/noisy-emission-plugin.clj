(ns ^{:doc "This map prints what's called."}
  midje.emission.plugins.noisy
  (:require [midje.emission.state :as state]))

(defn- in [function-name]
  (fn [& args] (println "In" function-name)))

(def emission-map {:pass (in 'pass)
                   :fail (in 'fail)
                   :starting-fact-stream (in 'forget-everything)
                   })

(state/install-emission-map emission-map)
