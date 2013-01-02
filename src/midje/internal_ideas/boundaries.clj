(ns ^{:doc "Execution boundaries that have to do with checking of faces"}
  midje.internal-ideas.boundaries
  (:require [midje.ideas.reporting.levels :as levelly]
            [midje.ideas.reporting.string-format :as string-format]))

(defn test-results-to-ternary [counters]
  (cond (every? zero? ((juxt :fail :pass) counters))
        nil

        (zero? (:fail counters))
        true
        
        :else
        false))


;; TODO: Once we reconcile the different ways results are checked and
;; returned by these two macros, extract commonality into a helper
;; macro.
(defmacro within-namespace-stream [ns-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (levelly/forget-past-results)
     ~@body
     (levelly/report-summary (ctf/run-clojure-test ~ns-sym))
     (string-format/previous-failure-count)))

(defmacro within-fact-function-stream [ffs-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (levelly/forget-past-results)
     ~@body
     (levelly/report-summary)
     (test-results-to-ternary (ctf/counters))))
