(ns ^{:doc "Functions dealing with making various forms of
            Midje output be ergonomically colorful."}
  midje.emission.colorize
  (:require [colorize.core :as color]
            [clojure.string :as str]
            [midje.config :as config]
            [midje.util.ecosystem :as ecosystem]))

;; This indirection is required so that the tests of this
;; file can fake the prerequisite
(def config-choice config/choice)

(defn- config-choice-as-string []
  (let [choice (config-choice :colorize)]
    (if (keyword? choice)
      (name choice)
      (str choice))))

(defn- colorize-string []
  (str/upper-case (or (ecosystem/getenv "MIDJE_COLORIZE")
                      (config-choice-as-string))))

(defn init! []
  (case (colorize-string)
    "TRUE"
    (do
      (def fail color/red)
      (def pass color/green)
      (def note color/cyan))

    "REVERSE"
    (do
      (def fail color/red-bg)
      (def pass color/green-bg)
      (def note color/cyan-bg))

    ;; else
    (do
      (def fail str)
      (def pass str)
      (def note str))))
