(ns midje.util.zip
  (:require [clojure.zip :as zip]))
  
(defn skip-to-rightmost-leaf [loc]
  "When positioned at leftmost position of branch, move to the end form.
   In a tree, that's the rightmost leaf."
  (let [end-form (zip/rightmost loc)]
    (if (zip/branch? end-form)
      (recur (zip/down end-form))
      end-form)))

(defn skip-down-then-rightmost-leaf [loc]
  "When positioned at a branch, move into it and then to the rightmost leaf."
  (skip-to-rightmost-leaf (zip/down loc)))

(defn n-times [n zip-fn loc]
  (if (zero? n)
    loc
    (recur (dec n) zip-fn (zip-fn loc))))

(defn remove-moving-right [loc]
  (-> loc zip/remove zip/next))

