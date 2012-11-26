(ns ^{:doc "Reporting that is influenced by print levels"}
  midje.ideas.reporting.levels
  (:use [midje.ideas.reporting.string-format :only [report-strings-summary
                                                    midje-position-string]]
        clojure.pprint)
  (:require midje.sweet
            [midje.clojure-test-facade :as ctf]
            [midje.util.colorize :as color]
            [clojure.string :as str]
            [midje.ideas.metadata :as metadata]
            [midje.util.form-utils :as form]))


(def ^{:private true} level-names [:print-nothing :print-no-summary :print-normally :print-namespaces :print-facts])
(def ^{:private true} levels      [-2        -1           0       1           2])
(def ^{:private true :testable true} names-to-levels (zipmap level-names levels))
(def ^{:private true :testable true} levels-to-names (zipmap levels level-names))

(defn separate-verbosity [args]
  (let [args (replace names-to-levels args)
        [print-level non-levels] (form/separate-by number? args)]
    [(or (first print-level) (names-to-levels :print-normally))
     non-levels]))

(defn report-best-fact-name [fact-function print-level]
  (when (>= print-level (names-to-levels :print-facts))
    (println (color/note
              (str "Checking "
                   (or (metadata/fact-name fact-function)
                       (metadata/fact-description fact-function)
                       (str "fact at " (midje-position-string
                                        [(metadata/fact-file fact-function)
                                         (metadata/fact-line fact-function)]))))))))

(defn report-summary [print-level]
  (when (> print-level (names-to-levels :print-no-summary))
    (report-strings-summary (ctf/counters))))

(def ^{:private true} last-namespace-shown (atom nil))

(defn report-changed-namespace [namespace print-level]
  (when (and (>= print-level (names-to-levels :print-namespaces))
             (not= namespace @last-namespace-shown))
      (println (color/note (str "= Namespace " namespace)))
      (swap! last-namespace-shown (constantly namespace))))

(defn forget-past-results []
    (ctf/zero-counters)
    (reset! last-namespace-shown nil))


