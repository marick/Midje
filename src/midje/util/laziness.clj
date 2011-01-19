;; -*- indent-tabs-mode: nil -*-

(ns midje.util.laziness)

(defn eagerly
  "Descend form, converting all lazy seqs into lists.
   Metadata is preserved. In the result all non-collections
   are identical? to those in the original form (as is
   their metadata). None of the collections are identical?
   even if they contain no lazy seqs."
  ;; Modified from clojure.walk/walk
  [form]
  (let [m #(with-meta % (meta form))]
    (cond (or (seq? form) (list? form))
          (m (apply list (map eagerly form)))
                          
          (vector? form)
          (m (vec (map eagerly form)))

          (map? form)
          (m (into form (map eagerly form)))

          (set? form)
          (m (into (if (sorted? form) (sorted-set) #{}) (map eagerly form)))

          :else
          form)))


