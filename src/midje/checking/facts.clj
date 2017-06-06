(ns ^{:doc "Facts as something you check"}
  midje.checking.facts
  (:require [midje.config :as config]
            [midje.emission.boundaries :as emission-boundary]
            [midje.data.compendium :as compendium]))

;;; Fact execution utilities

(defn check-one [fact]
  (let [top-level? (:midje/top-level-fact? (meta fact))
        fact-creation-filter (config/choice :fact-filter)]
    (cond (not top-level?)
          ;; Fact filter is irrelevant in nested functions.
          (emission-boundary/around-fact fact
            (fact))

          (not (fact-creation-filter fact))
          (str "This fact was ignored because of the current configuration. "
               "Only facts matching "
               (vec (map #(if (fn? %) "<some function>" %)
                         (:created-from (meta fact-creation-filter))))
               " will be created.")

          :else-top-level-fact-to-check
          (emission-boundary/around-top-level-fact fact {}
            ;; The fact is recorded on entry so that if a fact is
            ;; rechecked inside the midje.t-repl tests, the rechecked
            ;; fact stays the last-fact-checked (because it overwrites
            ;; the fact that's testing it).
            (compendium/record-fact-check! fact)
            (emission-boundary/around-fact fact
              (fact))))))

(defn creation-time-check [fact]
  (when (config/choice :check-after-creation)
    (check-one fact)))
