(ns ^{:doc "Zipper util functions."}
  midje.parsing.util.zip
  (:use [utilize.seq :only [first-truthy-fn]])
  (:require [clojure.zip :as zip]))


(defn translate-zipper
  "Traverses the zipper - for the first predicate that evaluates to truthy for matching a
  node, calls the corresponding translate function on that node. Then, continues traversing."
  [form & preds+translate-fns]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [truthy-fn (first-truthy-fn (take-nth 2 preds+translate-fns) loc)]
        (recur (zip/next ((get (apply hash-map preds+translate-fns) truthy-fn) loc)))
        (recur (zip/next loc))))))

(defn skip-to-rightmost-leaf
  "When positioned at leftmost position of branch, move to the end form.
   In a tree, that's the rightmost leaf."
  [loc]
  (let [end-form (zip/rightmost loc)]
    (if (zip/branch? end-form)
      (recur (zip/down end-form))
      end-form)))

(defn skip-down-then-rightmost-leaf
  "When positioned at a branch, move into it and then to the rightmost leaf."
  [loc]
  (skip-to-rightmost-leaf (zip/down loc)))

(defn n-times [n zip-fn loc]
  (if (zero? n)
    loc
    (recur (dec n) zip-fn (zip-fn loc))))

(defn remove-moving-right [loc]
  (-> loc zip/remove zip/next))

(defn previous-loc [loc]
  (zip/left (zip/up loc)))

(defn previous-form [loc]
  (zip/node (previous-loc loc)))
  
