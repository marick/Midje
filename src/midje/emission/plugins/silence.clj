(ns ^{:doc "A template for emission maps that emit nothing."}
  midje.emission.plugins.silence
  (:require [midje.emission.state :as state]))

(defn ignore [& args])

(defn make-map [& keys]
  (zipmap keys (repeat ignore)))

(def emission-map (make-map :pass
                            :fail
                            :starting-to-check-top-level-fact
                            :finishing-top-level-fact
                            :starting-to-check-fact
                            :finishing-fact
                            :possible-new-namespace
                            :finishing-fact-stream
                            :future-fact
                            :starting-fact-stream))

(state/install-emission-map emission-map)
