(ns ^{:doc "Execution boundaries that have to do with checking of faces"}
  midje.emission.boundaries
  (:require [midje.clojure-test-facade :as ctf]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]))

(defn midje-results-to-ternary [] ; note: used when clojure.test results are irrelevant
  (let [failures (state/output-counters:midje-failures)
        passes (state/output-counters:midje-passes)]
    (cond (every? zero? [failures passes])
          nil

          (zero? failures)
          true
        
          :else
          false)))

(defn all-test-failures-to-self-documenting-map [ct-counters]
  {:failures (+ (state/output-counters:midje-failures) (:fail ct-counters))})
  

;; TODO: Once we reconcile the different ways results are checked and
;; returned by these two macros, extract commonality into a helper
;; macro.
(defmacro around-namespace-stream [ns-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-everything)
     ~@body
     (let [ct-counters# (ctf/run-tests ~ns-sym)]
       (emit/fact-stream-summary ct-counters#)
       (all-test-failures-to-self-documenting-map ct-counters#))))

(defmacro around-fact-function-stream [ffs-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-everything)
     ~@body
     (emit/fact-stream-summary)
     (midje-results-to-ternary)))


;; TODO: Note that both of the fact around functions need to check
;; failure counts. It would perhaps make sense to have the
;; `around-fact-function` wrap `around-top-level-fact-function`, but I
;; think it would surprise and annoy plugin-writers.

(defmacro around-top-level-fact-function [ff-sym & body]
  `(let [starting-failures# (state/output-counters:midje-failures)]
     (emit/possible-new-namespace (metadata/fact-namespace ~ff-sym))
     (emit/starting-to-check-top-level-fact ~ff-sym)
     ~@body
     (= starting-failures# (state/output-counters:midje-failures))))

(defmacro around-fact-function [ff-sym & body]
  `(let [starting-failures# (state/output-counters:midje-failures)]
     (emit/starting-to-check-fact ~ff-sym)
     (fact-context/adds (metadata/fact-description ~ff-sym)
                        ~@body)
     (= starting-failures# (state/output-counters:midje-failures))))


(defmacro around-check [& body]
  `(do ~@body))


  
