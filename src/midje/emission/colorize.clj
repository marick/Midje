(ns ^{:doc "Functions dealing with making various forms of
            Midje output be ergonomically colorful."}
  midje.emission.colorize
  (:require [colorize.core :as color]
            [midje.config :as config]))

;; This indirection is required so that the tests of this
;; file can fake the prerequisite
(def config-choice config/choice)

(defn- build-colorizer
  [normal-color reverse-color]
  (fn [& s]
    (apply (case (config-choice :colorize)
             :true normal-color
             :reverse reverse-color
             str)
           s)))

(def fail (build-colorizer color/red color/red-bg))

(def pass (build-colorizer color/green color/green-bg))

(def note (build-colorizer color/cyan color/cyan-bg))
