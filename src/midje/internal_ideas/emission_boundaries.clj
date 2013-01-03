(ns ^{:doc "Execution boundaries that have to do with checking of faces"}
  midje.internal-ideas.emission-boundaries
  (:require [midje.ideas.reporting.levels :as levelly]
            [midje.ideas.reporting.string-format :as string-format]
            [midje.internal-ideas.emissions :as emit]))

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
(defmacro around-namespace-stream [ns-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-complete-past)
     ~@body
     (levelly/report-summary (ctf/run-clojure-test ~ns-sym))
     (string-format/previous-failure-count)))

(defmacro around-fact-function-stream [ffs-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-complete-past)
     ~@body
     (levelly/report-summary)
     (test-results-to-ternary (ctf/counters))))


(defmacro around-fact-function [ff-sym & body]
  `(do 
     (levelly/report-checking-fact ~ff-sym)
     (fact-context/adds (metadata/fact-description ~ff-sym)
                        ~@body)))

(defmacro around-top-level-fact-function [ff-sym & body]
  `(do
     (#'midje.ideas.reporting.report/fact-begins)
     (levelly/report-changed-namespace (metadata/fact-namespace ~ff-sym))
     ~@body
     (#'midje.ideas.reporting.report/fact-checks-out?)))

(defmacro around-check [& body]
  `(do ~@body))


(defmacro around-isolated-emission-context [new-emission-functions & body]
  `(binding [emit/counters (atom emit/fresh-counters)
             emit/emission-functions ~new-emission-functions]
     ~@body))
  
