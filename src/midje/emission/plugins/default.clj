(ns ^{:doc "The default for Midje output"}
  midje.emission.plugins.default
  (:use [midje.util.form-utils :only [midje-position-string]])
  (:require [midje.ideas.reporting.levels :as levelly]
            [midje.clojure-test-facade :as ctf]
            [midje.util.colorize :as color]
            [midje.emission.state :as state]
            [midje.ideas.metadata :as metadata]
            [midje.emission.plugins.silence :as silence]))

(defn fail [report-map]
  (clojure.test/report report-map))

(defn starting-to-check-fact [fact-function]
  (ctf/output (color/note
               (str "Checking "
                    (or (metadata/fact-name fact-function)
                        (metadata/fact-description fact-function)
                        (str "fact at " (midje-position-string
                                         [(metadata/fact-file fact-function)
                                          (metadata/fact-line fact-function)])))))))

(def emission-map (merge silence/emission-map
                         {:fail fail
                          :starting-to-check-fact starting-to-check-fact
                          :forget-everything #(reset! levelly/last-namespace-shown nil)
                          }))

(state/install-emission-map emission-map)
