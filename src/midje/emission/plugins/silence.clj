(ns ^{:doc "A template for emission maps that emit nothing."}
  midje.emission.plugins.silence
  (:require [midje.emission.state :as state]))

(defn ignore [& args])

(def emission-map {:pass ignore
                   :fail ignore
                   :starting-to-check-fact ignore
                   :possible-new-namespace ignore
                   :forget-everything ignore
                   })

(state/install-emission-map emission-map)
