(ns ^{:doc "Functions dealing with making various forms of 
            Midje output be ergonomically colorful."}
  midje.emission.colorize
  (:require [colorize.core :as color]
            [clojure.string :as str]
            [midje.config :as config])
  (:use [midje.util.ecosystem :only [getenv on-windows?]]))

(defn colorize-setting []
  (config/choice :colorize))

(defn- colorize-config-as-str []
  (let [setting-as-str (str (colorize-setting))]
    (when-not (str/blank? setting-as-str) setting-as-str)))

(defn- colorize-choice []
  (str/upper-case (or (getenv "MIDJE_COLORIZE")
                      (colorize-config-as-str)
                      (str (not (on-windows?))))))

(defn init! []
  (case (colorize-choice)
    "TRUE" (do
             (def fail color/red)
             (def pass color/green)
             (def note color/cyan))

    ("REVERSE" ":REVERSE") (do
                (def fail color/red-bg)
                (def pass color/green-bg)
                (def note color/cyan-bg))

    (do
      (def fail str)
      (def pass str)
      (def note str))))
