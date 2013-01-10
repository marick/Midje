(ns ^{:doc "The default for Midje output"}
  midje.emission.plugins.default
  (:use [midje.util.form-utils :only [midje-position-string]])
  (:require [midje.clojure-test-facade :as ctf]
            [midje.util.colorize :as color]
            [midje.emission.state :as state]
            [midje.ideas.metadata :as metadata]
            [midje.emission.plugins.silence :as silence]))

(defn fail [report-map]
  (clojure.test/report report-map))


(def last-namespace-shown (atom nil))

(defn set-last-namespace-shown! [string]
  (reset! last-namespace-shown string))

(defn possible-new-namespace [namespace-symbol]
  (when (not= namespace-symbol @last-namespace-shown)
    (println (color/note (str "= Namespace " namespace-symbol)))
    (set-last-namespace-shown! namespace-symbol)))

(defn forget-everything []
  (set-last-namespace-shown! nil))

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
                          :possible-new-namespace possible-new-namespace
                          :forget-everything forget-everything
                          }))

(state/install-emission-map emission-map)
