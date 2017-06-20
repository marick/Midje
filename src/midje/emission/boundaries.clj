(ns ^{:doc "Execution boundaries that have to do with checking of faces"}
  midje.emission.boundaries
  (:require [midje.data.fact :as fact]
            [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.api :as emit]
            [midje.data.nested-facts :as nested-facts]
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
  {:failures (+ (state/output-counters:midje-failures)
                (:fail ct-counters)
                (:error ct-counters))})


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

(defmacro around-fact-stream [ffs-sym config-settings & body]
  `(config/with-augmented-config ~config-settings
     (emit/forget-everything)
     ~@body
     (emit/fact-stream-summary)
     (midje-results-to-ternary)))


;; TODO: Note that both of the fact around functions need to check
;; failure counts. It would perhaps make sense to have the
;; `around-fact` wrap `around-top-level-fact`, but I
;; think it would surprise and annoy plugin-writers.

(defmacro around-top-level-fact [fact-sym & body]
  `(let [starting-failures# (state/output-counters:midje-failures)]
     (emit/possible-new-namespace (fact/namespace ~fact-sym))
     (emit/starting-to-check-top-level-fact ~fact-sym)
     ~@body
     (emit/finishing-top-level-fact ~fact-sym)
     (= starting-failures# (state/output-counters:midje-failures))))

(defmacro around-fact [fact-sym & body]
  `(let [starting-failures# (state/output-counters:midje-failures)]
     (emit/starting-to-check-fact ~fact-sym)
     (nested-facts/in-new-fact ~fact-sym ~@body)
     (emit/finishing-fact ~fact-sym)
     (= starting-failures# (state/output-counters:midje-failures))))


(defmacro around-check [& body]
  `(do ~@body))



