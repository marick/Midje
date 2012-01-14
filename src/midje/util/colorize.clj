;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Functions dealing with making various forms of 
            Midje output be ergonomically colorful."}
  midje.util.colorize
  (:require [colorize.core :as color]
            [clojure.string :as str])
  (:use [midje.util.ecosystem :only [getenv on-windows?]]))


(defn colorize-choice []
  (str/upper-case (or (getenv "MIDJE_COLORIZE")
                    (str (not (on-windows?))))))

(case (colorize-choice)
  "TRUE" (do
           (def fail color/red)
           (def pass color/green)
           (def note color/cyan))

  "REVERSE" (do
              (def fail color/red-bg)
              (def pass color/green-bg)
              (def note color/cyan-bg))

  (do
    (def fail identity)
    (def pass identity)
    (def note identity)))

(defn colorize-deftest-output [s]
  (-> s 
      (str/replace #"^FAIL" (fail "FAIL"))
      (str/replace #"^ERROR" (fail "ERROR"))))