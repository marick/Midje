(ns ^{:doc "Facts as something you check"}
    midje.checking.facts
  (:require [midje.config :as config]
            [midje.emission.api :as emit]
            [midje.emission.boundaries :as emission-boundary]
            [midje.data.compendium :as compendium]
            [midje.data.fact :as fact]
            [midje.data.nested-facts :as nested-facts]
            [pointer.core :as pointer]))

;;; Fact execution utilities

(defn log-exceptions [fact]
  (try
     (fact)
     (catch Throwable t
       (let [throw-data       (ex-data t)
             inner-throwable? (= (:type throw-data) :exception-already-logged)]
         (cond (and inner-throwable? (fact/top-level-fact? fact))
               (throw (:original-exception throw-data))

               inner-throwable?
               (throw t)

               :else
               (do
                 (emit/fail {:type        :exception-inside-fact
                             :description (nested-facts/descriptions)
                             :namespace   (fact/namespace fact)
                             :position    (pointer/line-number-known (fact/line fact))})
                 (throw (ex-info "intermedia midje exception"
                                 {:original-exception t
                                  :type               :exception-already-logged}))))))))

(defn check-one [fact]
  (let [fact-creation-filter (config/choice :fact-filter)]
    (cond (not (fact/top-level-fact? fact))
          ;; Fact filter is irrelevant in nested functions.
          (emission-boundary/around-fact fact
                                         (log-exceptions fact))

          (not (fact-creation-filter fact))
          (str "This fact was ignored because of the current configuration. "
               "Only facts matching "
               (vec (map #(if (fn? %) "<some function>" %)
                         (:created-from (meta fact-creation-filter))))
               " will be created.")

          :else-top-level-fact-to-check
          (emission-boundary/around-top-level-fact fact
            ;; The fact is recorded on entry so that if a fact is
            ;; rechecked inside the midje.t-repl tests, the rechecked
            ;; fact stays the last-fact-checked (because it overwrites
            ;; the fact that's testing it).
            (compendium/record-fact-check! fact)
            (emission-boundary/around-fact fact
                                           (log-exceptions fact))))))

(defn creation-time-check [fact]
  (when (config/choice :check-after-creation)
    (check-one fact)))
