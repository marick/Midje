(ns ^{:doc "TAP output. See http://testanything.org"}
  midje.emission.plugins.tap
  (:require [clojure.string :as str]
            [midje.data.fact :as fact]
            [midje.emission.plugins.util :as util]
            [midje.emission.plugins.silence :as silence]
            [midje.emission.plugins.default-failure-lines :as lines]
            [midje.emission.state :as state]
            [midje.emission.util :refer :all]))

(def fact-counter (atom 0))

(defn- format-failure-summary [failure-map]
  (let [[_ _ & more] (lines/summarize failure-map)]
    (map #(str "# " %) (conj more (apply str (:position failure-map))))))

(defn future-fact [description-list position]
  (swap! fact-counter inc)
  (util/emit-one-line (str "not ok " @fact-counter
                           " # TODO "
                           (when-let [doc (util/format-nested-descriptions description-list)]
                             (str (pr-str doc) " "))
                           "at " (util/filename-lineno position))))
(defn fail [failure-map]
  (swap! fact-counter inc)
  (util/emit-lines (format-failure-summary failure-map))
  (util/emit-one-line
   (str "not ok " @fact-counter)))

(defn pass []
  (swap! fact-counter inc)
  (util/emit-one-line (str "ok " @fact-counter)))

(def last-namespace-shown (atom nil))

(defn set-last-namespace-shown! [string]
  (reset! last-namespace-shown string))

(defn possible-new-namespace [namespace-symbol]
  (when (not= namespace-symbol @last-namespace-shown)
    (util/emit-one-line (str "# Namespace " namespace-symbol))
    (set-last-namespace-shown! namespace-symbol)))

(defn starting-fact-stream []
  (reset! fact-counter 0)
  (set-last-namespace-shown! nil))

(defn starting-to-check-fact [fact]
  (util/emit-one-line (str "# Checking "
                           (or (fact/name fact)
                               (fact/description fact)
                               (str "fact at " (midje-position-string
                                                [(fact/file fact)
                                                 (fact/line fact)]))))))


(defn finishing-fact-stream [midje-counters _]
  (let [passes-and-failures (+ (:midje-passes midje-counters 0)
                               (:midje-failures midje-counters 0))]
    (util/emit-one-line (str 1 ".." @fact-counter
                             " # midje count: " passes-and-failures
                             (when (= passes-and-failures 0)
                               "# No facts were checked. Is that what you wanted?")))))

(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))


(def emission-map (merge silence/emission-map
                         (make-map :fail
                                   :pass
                                   :future-fact
                                   :starting-to-check-fact
                                   :possible-new-namespace
                                   :finishing-fact-stream
                                   :starting-fact-stream)))

(state/install-emission-map emission-map)
