(ns midje.util.sequence
  (:use [ordered.map :only (ordered-map)]))

(defn zip [& seqs]
  (apply map list seqs))

(defn ordered-zipmap [keys vals]
  "like zipmap, but guarantees order of the entries"
  (loop [m (ordered-map)
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs))
             (next ks)
             (next vs))
      m)))

(defn split-by-pred [pred coll]
  (list (filter pred coll) (remove pred coll)))
