(ns ^{:doc "Zipper util functions."}
  midje.parsing.util.zip
  (:require [clojure.zip :as zip]))

;;; Copied from utilize to remove dependencies.
;;; https://github.com/AlexBaranosky/Utilize
(defn unchunk
  "Force a lazy sequence to not use size 32 chunks, but true one-element laziness"
  [s]
  (lazy-seq (when-let [s (seq s)]
              (cons (first s) (unchunk (rest s))))))

(defn first-truthy-fn
  "Returns the first function in a seq of functions
  that evaluates to truthy for the given arguments - it shortcircuits,
  only evaluating the minimum number of functions necessary"
  [preds & args]
  (first (filter #(apply % args) (unchunk preds))))
;;; end

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

