(ns midje.util.laziness
  "To evaluate a fact it needs to be eagerly evaluated."
  (:require [such.control-flow :refer [branch-on]]))

(defn eagerly
  "Descend form, converting all lazy seqs into lists.
   Metadata is preserved. In the result all non-collections
   are identical? to those in the original form (as is
   their metadata). None of the collections are identical?
   even if they contain no lazy seqs."
  ;; Modified from clojure.walk/walk
  [form]
  (let [m (fn [x] (if (instance? clojure.lang.IObj x) (with-meta x (meta form)) x))]
    (branch-on form
      (some-fn seq? list?)    (m (apply list (map eagerly form)))
      vector?                 (m (vec (map eagerly form)))
      map?                    (m (into form (map eagerly form)))
      set?                    (m (into (empty form) (map eagerly form)))
      :else                   form)))
