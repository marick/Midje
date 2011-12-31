;; -*- indent-tabs-mode: nil -*-

(ns midje.util.colorize
  (:require [colorize.core :as color])
  (:use [midje.util.ecosystem :only [getenv on-windows?]]))


(defn colorize-choice []
  (.toUpperCase (or (getenv "MIDJE_COLORIZE")
                    (str (not (on-windows?))))))

(defn- default-color-choice? []
  (= (colorize-choice) "TRUE"))

(defn reverse-color-choice? []
  (= (colorize-choice) "REVERSE"))

(defn colorizing? [] 
  (not (= (colorize-choice) "FALSE")))

(cond (default-color-choice?)
      (do
        (def fail color/red)
        (def pass color/green)
        (def note color/cyan))

      (reverse-color-choice?)
      (do
        (def fail color/red-bg)
        (def pass color/green-bg)
        (def note color/cyan-bg))

      :else
      (do
        (def fail identity)
        (def pass identity)
        (def note identity)))

(defn colorized? [s] 
  (.startsWith s "\033["))
      
