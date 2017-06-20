(ns ^{:doc "Progress formatter for Midje output"}
  midje.emission.plugins.progress
  (:require [midje.emission.colorize :as color]
            [midje.emission.plugins.util :as util]
            [midje.emission.plugins.silence :as silence]
            [midje.emission.plugins.default :as default]
            [midje.emission.plugins.default-failure-lines :as lines]
            [midje.emission.state :as state]
            [midje.emission.util :refer :all]))

(defn- def-failure-cache []
 (defonce raw-failures-cache (atom [])))

(defn pass []
  (print (color/pass "."))
  (flush))

(defn fail [failure-map]
  (print (color/fail "F"))
  (flush))

(defn future-fact [description-list position]
  (print (color/note "P"))
  (flush))

(defn starting-fact-stream []
  (def-failure-cache)
  (let [failures (state/raw-fact-failures)]
    (when-not (empty? failures)
      (swap! raw-failures-cache concat failures))))

(defn finishing-fact-stream [midje-counters clojure-test-map]
  (doseq [failure-map @raw-failures-cache]
    (println)
    (util/emit-lines (lines/summarize failure-map))
    (flush))
  (println)
  (default/finishing-fact-stream midje-counters clojure-test-map))

(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))

(def emission-map (merge silence/emission-map
                         (make-map :fail
                                   :pass
                                   :future-fact
                                   :starting-fact-stream
                                   :finishing-fact-stream)))

(state/install-emission-map emission-map)
