(ns ^{:doc "Reporting that is influenced by print levels"}
  midje.ideas.reporting.levels
  (:use [midje.ideas.reporting.string-format :only [report-strings-summary
                                                    ]]
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
  (let [[[print-level & extras] non-levels] (form/separate-by valids args)]
    (when (seq extras)
      (throw (user-error "You have extra print level names or numbers.")))
    (dorun (map validate-level! (filter number? args)))
      
    (if print-level
      [[print-level]  print-level                   non-levels]
      [[           ]  (config/choice :print-level)  non-levels])))


(defn level-checker [operation]
  (fn [level-name]
    (operation (normalize (config/choice :print-level))
               (normalize level-name))))

(def at-or-above? (level-checker >=))
(def above?(level-checker >))

