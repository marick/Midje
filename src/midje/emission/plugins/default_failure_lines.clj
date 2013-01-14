(ns ^{:doc "How the default emitter reports on failures"}
  midje.emission.plugins.default-failure-lines
  (:use midje.emission.plugins.util)
  (:require [midje.clojure-test-facade :as ctf]))

(defmulti messy-lines :type)

(defmethod messy-lines :mock-expected-result-failure [m]
  (list
   (failure-notice m)
   (str "    Expected: " (:expected-form-to-print m))
   (str "      Actual: " (attractively-stringified-form (:actual m)))))

(defmethod messy-lines :default [failure-map]
  (midje.ideas.reporting.string-format/report-strings failure-map))

(defn summarize [failure-map]
  (linearize-lines (messy-lines failure-map)))

