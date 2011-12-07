;; -*- indent-tabs-mode: nil -*-

(ns midje.util.colorize
  (:require [colorize.core :as color])
  (:use [midje.util.ecosystem :only [getenv]]))


(defn- colorize-choice []
  (.toUpperCase (or (getenv "MIDJE_COLORIZE") "true")))

(defn- default-color-choice? []
  (= (colorize-choice) "TRUE"))

(defn reverse-color-choice? []
  (= (colorize-choice) "REVERSE"))

(cond (default-color-choice?)
      (do
        (def fail-color color/red)
        (def note-color color/cyan))

      (reverse-color-choice?)
      (do
        (def fail-color color/red-bg)
        (def note-color color/cyan-bg))

      :else
      (do
        (def fail-color identity)
        (def note-color identity)))
      
