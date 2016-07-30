(ns midje.emission.plugins.flare
  (:require [commons.clojure.core :refer :all :exclude [any?]]
            [midje.util.ecosystem :as ecosystem]
            [midje.emission.plugins.util :as util]))

(defn emit-flare-lines [& args])

(require '[flare.core :as flare])
 ;; Following code adapted from
 ;; https://raw.githubusercontent.com/andersfurseth/flare/master/src/flare/midje.clj

 (defn report [reports]
   (when (seq reports)
     (util/emit-one-line "")
     (doseq [report reports]
       (util/emit-one-line report))))

 (defn generate-reports [diffs]
   ; (prn "Flare data: " diffs)
   ; (clojure.pprint/pprint diffs)
   (println "Flare output (experimental):")
   (flare/generate-reports diffs))

 (defn emit-flare-lines [failure-map]
   (when (= (:type failure-map) :actual-result-did-not-match-expected-value)
      (some-> (flare/diff (:expected-result failure-map) (:actual failure-map))
              generate-reports
              report)))
