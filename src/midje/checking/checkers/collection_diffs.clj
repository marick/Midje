(ns midje.checking.checkers.collection-diffs
  "Checkers for collections and strings."
  (:require [midje.util.pile :as pile]
            [midje.checking.checkers.collection :as old]
            [midje.checking.core :as core]
            [such.shorthand :refer [not-empty?]]
            flare.map)
  (:import [flare.map MapKeysDiff]
           [flare.atom AtomDiff]))

;; Note: checkers need to be exported in ../checkers.clj

;; TODO: Currently not handling "looseness"

(defn classify [actual expected looseness]
  (cond (and (map? actual) (map? expected))  :map:map
        :else :unknown))

(defmulti contains:diffs (fn [actual expected looseness] (classify actual expected looseness)))

(defmethod contains:diffs :map:map [actual-map expected-map _]
  (let [[actual-keys expected-keys] (map #(set (keys %)) [actual-map expected-map])
        missing-keys (clojure.set/difference expected-keys actual-keys)

        diff-map (reduce (fn [so-far shared-key]
                           (let [[actual-val expected-val] (map shared-key [actual-map expected-map])
                                 comparison (core/extended-= actual-val expected-val)]
                             (cond (core/extended-true? comparison)
                                   so-far

                                   (core/data-laden-falsehood? comparison)
                                   (do
                                     (prn "data laden falsehood " comparison)
                                     (assoc so-far shared-key (new AtomDiff actual-val expected-val)))

                                   :else
                                   (assoc so-far shared-key (new AtomDiff actual-val expected-val)))))
                          {}
                          (clojure.set/difference expected-keys missing-keys))]
    (-> []
        (cond-> (not-empty? missing-keys) (conj (new MapKeysDiff missing-keys nil))
                (not-empty? diff-map) (conj diff-map)))))
