(ns midje.util.colorize
  (:require [colorize.core :as color])
  (:use [midje.util.ecosystem :only [getenv]]))

(let [colorize-env-var (getenv "MIDJE_COLORIZE") ;; keep to just one env var lookup
      midje-colorize (fn [colorize-fn]
                       (if (or (nil? colorize-env-var) (Boolean/valueOf colorize-env-var))
                         colorize-fn
                         identity))]

  (def red (midje-colorize color/red))
  (def yellow (midje-colorize color/yellow)))