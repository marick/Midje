(ns ^{:doc "The default for Midje output"}
  midje.emission.plugins.default
  (:require 
            [midje.ideas.reporting.levels :as levelly]
            [midje.emission.state :as state]
            [midje.emission.plugins.silence :as silence]))


(def emission-map (merge silence/emission-map
                         {:fail (fn [report-map]
                                  (clojure.test/report report-map))
                          :forget-everything #(reset! levelly/last-namespace-shown nil)
                          }))

(state/install-emission-map emission-map)
