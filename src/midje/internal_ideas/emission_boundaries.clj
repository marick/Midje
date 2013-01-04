(ns ^{:doc "Execution boundaries that have to do with checking of faces"}
  midje.internal-ideas.emission-boundaries
  (:require [midje.ideas.reporting.levels :as levelly]
            [midje.ideas.reporting.string-format :as string-format]
            [midje.internal-ideas.emissions :as emit]
            [midje.internal-ideas.state :as state]))

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
     (levelly/report-summary (ctf/run-tests ~ns-sym))
     (string-format/previous-failure-count)))

(defmacro around-fact-function-stream [ffs-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-complete-past)
     ~@body
     (levelly/report-summary)
     (test-results-to-ternary (ctf/counters))))


;; TODO: Note that both of the fact around functions
;; need to check failure counts. Perhaps instead of
;; around-top-level being wrapped around around-fact,
;; it should be the other way around?
(defmacro around-fact-function [ff-sym & body]
  `(let [starting-failures# (emit/midje-failures)]
     (levelly/report-checking-fact ~ff-sym)
     (fact-context/adds (metadata/fact-description ~ff-sym)
                        ~@body)
     (= starting-failures# (emit/midje-failures))))


(defmacro around-top-level-fact-function [ff-sym & body]
  `(let [starting-failures# (emit/midje-failures)]
     (levelly/report-changed-namespace (metadata/fact-namespace ~ff-sym))
     ~@body
     (= starting-failures# (emit/midje-failures))))

(defmacro around-check [& body]
  `(do ~@body))


(defmacro around-isolated-emission-context [new-emission-functions & body]
  `(binding [state/output-counters (atom state/fresh-output-counters)
             state/emission-functions ~new-emission-functions]
     (state/reset-output-counters!)
     ~@body))
  
