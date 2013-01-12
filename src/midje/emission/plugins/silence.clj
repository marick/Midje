(ns ^{:doc "A template for emission maps that emit nothing."}
  midje.emission.plugins.silence
  (:require [midje.emission.state :as state]))

(defn ignore [& args])

(defn make-map [& keys]
  (zipmap keys (repeat ignore)))

(def emission-map (make-map :pass
                            :fail
                            :starting-to-check-fact
                            :possible-new-namespace
                            :fact-stream-summary
                            :future-fact
                            :forget-everything))

(state/install-emission-map emission-map)
