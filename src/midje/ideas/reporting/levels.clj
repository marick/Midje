(ns ^{:doc "Reporting that is influenced by print levels"}
  midje.ideas.reporting.levels
  (:use [midje.ideas.reporting.string-format :only [report-strings-summary
                                                    midje-position-string]]
        [midje.error-handling.exceptions :only [user-error]]
        midje.ideas.reporting.level-defs
        clojure.pprint)
  (:require [midje.clojure-test-facade :as ctf]
            [midje.util.colorize :as color]
            [midje.config :as config]
            [clojure.string :as str]
            [midje.ideas.metadata :as metadata]
            [midje.util.form-utils :as form]))

(defn separate-print-levels [args]
  (let [args (replace names-to-levels args)
        [[print-level & extras] non-levels] (form/separate-by number? args)]
    (when (seq extras)
      (throw (user-error "You have extra print level names or numbers.")))
    [(normalize (or print-level (config/choice :print-level)))
     non-levels]))


(defn level-checker [operation]
  (fn [level-name]
    (operation (normalize (config/choice :print-level))
               (normalize level-name))))

(def at-or-above? (level-checker >=))
(def above?(level-checker >))

(defn report-checking-fact [fact-function]
  (when (at-or-above? :print-facts)
    (println (color/note
              (str "Checking "
                   (or (metadata/fact-name fact-function)
                       (metadata/fact-description fact-function)
                       (str "fact at " (midje-position-string
                                        [(metadata/fact-file fact-function)
                                         (metadata/fact-line fact-function)]))))))))

(defn report-summary []
  (when (above? :print-no-summary)
    (report-strings-summary (ctf/counters))))

(def last-namespace-shown (atom nil))

(defn report-changed-namespace [namespace]
  (when (and (at-or-above? :print-namespaces)
             (not= namespace @last-namespace-shown))
    (println (color/note (str "= Namespace " namespace)))
    (swap! last-namespace-shown (constantly namespace))))

(defn forget-past-results []
    (ctf/zero-counters)
    (reset! last-namespace-shown nil))


