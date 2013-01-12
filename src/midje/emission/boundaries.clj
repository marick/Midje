(ns ^{:doc "Execution boundaries that have to do with checking of faces"}
  midje.emission.boundaries
  (:require [midje.ideas.reporting.string-format :as string-format]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]))

(defn test-results-to-ternary [counters]
  (let [failures (state/output-counters:midje-failures)
        passes (state/output-counters:midje-passes)]
    (cond (every? zero? [failures passes])
          nil

          (zero? failures)
          true
        
          :else
          false)))

;; TODO: Once we reconcile the different ways results are checked and
;; returned by these two macros, extract commonality into a helper
;; macro.
(defmacro around-namespace-stream [ns-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-everything)
     ~@body
     (emit/fact-stream-summary (ctf/run-tests ~ns-sym))
     (string-format/previous-failure-count)))

(defmacro around-fact-function-stream [ffs-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-everything)
     ~@body
     (emit/fact-stream-summary)
     (test-results-to-ternary (ctf/counters))))


;; TODO: Note that both of the fact around functions
;; need to check failure counts. Perhaps instead of
;; around-top-level being wrapped around around-fact,
;; it should be the other way around?
(defmacro around-fact-function [ff-sym & body]
  `(let [starting-failures# (emit/midje-failures)]
     (emit/starting-to-check-fact ~ff-sym)
     (fact-context/adds (metadata/fact-description ~ff-sym)
                        ~@body)
     (= starting-failures# (emit/midje-failures))))


(defmacro around-top-level-fact-function [ff-sym & body]
  `(let [starting-failures# (emit/midje-failures)]
     (emit/possible-new-namespace (metadata/fact-namespace ~ff-sym))
     ~@body
     (= starting-failures# (emit/midje-failures))))

(defmacro around-check [& body]
  `(do ~@body))


  
