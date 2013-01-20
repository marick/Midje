(ns ^{:doc "Facts as a data structure"}
  midje.data.fact
  (:refer-clojure :exclude [name namespace])
  (:require [midje.config :as config]
            [midje.emission.boundaries :as emission-boundary]
            [midje.internal-ideas.fact-context :as fact-context]
            [midje.internal-ideas.compendium :as compendium]))
                  
;;; This is a potential target for migrating the non-parsing,
;;; non-checking parts of ideas/facts.clj, ideas/metadata.clj. It's
;;; sketchy now, since it's only being used to keep the emission
;;; plugin code from depending on code slated to be destroyed.

(def fact-properties
  [:midje/source :midje/file :midje/line :midje/namespace
   :midje/name :midje/description :midje/top-level?])

(defn make-getters 
  "Create midje.data.fact/name midje.data.fact/file, etc."
  [in-namespace prefix]
  (doseq [property-key fact-properties]
    (let [property-string (clojure.core/name property-key)
          function-symbol (symbol (str prefix property-string))

          annotated-function-symbol
          (vary-meta function-symbol assoc
                     :arglists '([fact])
                     :doc (str "Return the " property-string
                               " of the given fact.\n  Facts are in the form returned by `fetch-facts`."))]
      (intern *ns*
              annotated-function-symbol
              (comp property-key meta)))))
(make-getters *ns* "")


;;; Fact execution utilities

(defn check-one [fact]
  (let [top-level? (:midje/top-level-fact? (meta fact))
        fact-creation-filter (config/choice :desired-fact?)]
    (cond (not (fact-creation-filter fact))
          (str "This fact was ignored because of the current configuration. "
               "Only facts matching "
               (vec (map #(if (fn? %) "<some function>" %) 
                         (:created-from (meta fact-creation-filter))))
               " will be created.")
            
          (not top-level?)
          (emission-boundary/around-fact fact
            (fact))

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
