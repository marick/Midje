;; -*- indent-tabs-mode: nil -*-

(ns midje.util.form-utils
  (:use [midje.util.treelike :only [tree-variant]]
        [clojure.set :only [difference]]
        [tmp.ordered.map :only [ordered-map]])
  (:require [clojure.zip :as zip]))

(defn regex? [thing]
  (= (class thing) java.util.regex.Pattern))

(defn classic-map? [x]
  (.isInstance clojure.lang.APersistentMap x))
  
(defn record? [x]
  (and (map? x) (not (classic-map? x))))
  
(defn symbol-named?
  "Is the thing a symbol with the name given by the string?"
  [thing string]
  (and (symbol? thing)
       (= (name thing) string)))

(defn first-named?
  "Is the form's first element a symbol whose name is the desired string?"
  [form desired]
  (and (sequential? form)
       (symbol-named? (first form) desired)))

(defmulti quoted? tree-variant)
(defmethod quoted? :zipper [loc]
  (quoted? (zip/node loc)))
(defmethod quoted? :form [form]
  (first-named? form "quote"))

(defn preserve-type
  "If the original form was a vector, make the transformed form a vector too."
  [original-form transformed-form]
  (if (vector? original-form)
    (vec transformed-form)
    transformed-form))

(defn separate-by
  "Like clojure.core/separate, but not lazy, returns nil for empty list."
  ;; TODO: One of those two differences is the difference between
  ;; a blowup in PersistentArrayMap and no blowup. Should investigate.
  [predicate forms]
  (let [ensure-truthful (comp not not predicate)]
    (let [group (group-by ensure-truthful forms)]
      [ (group true) (group false) ])))

(defn reader-line-number 
  "Find what line number the reader put on the given form or on
   one of its elements. If no line numbers, a warning string."
  [form]
  (or (:line (meta form))
      (some (comp :line meta) form)
      "0 (no line info)"))
  
(defn flatten-and-remove-nils [seq]
  (->> seq flatten (remove nil?)))

(defn vector-without-element-at-index [index v]
  (vec (concat (subvec v 0 index) (subvec v (inc index)))))

(defn tack-on-to [hashmap & kvs]
  "Conj new values onto appropriate keys of a map"
  (merge-with conj hashmap (apply (partial assoc {}) kvs)))

(defn pairs [first-seq second-seq]
  "Return [ (first first-seq) (first second-seq)] ..."
  (partition 2 (interleave first-seq second-seq)))

(defn hash-map-duplicates-ok [& keys-and-vals]
  "Like hash-map, except duplicate keys are OK. Last one takes precedence."
  (if (empty? keys-and-vals)
    {}
    (apply (partial assoc {}) keys-and-vals)))

(defn apply-pairwise [ functions & arglists ]
  "(apply-pairwise [inc dec] [1 1] [2 2]) => [ [2 0] [3 1] ]
   Note that the functions must take only a single argument."
  (map (partial map 
  		(fn [f arg] (f arg)) 
  		functions) 
  	arglists))

(defn map-difference [bigger smaller]
  (select-keys bigger (difference (set (keys bigger)) (set (keys smaller)))))

(defn ordered-zipmap [keys vals]
  "Like zipmap, but guarantees order of the entries"
  (loop [m (ordered-map)
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs)) 
             (next ks)
             (next vs))
      m)))

(defn first-truthy-fn
  "Returns the first function in a seq of functions
  that evaluates to truthy for the given arguments"
  [[pred & more-preds] & args]
  (when pred
    (if (apply pred args)
      pred
      (apply first-truthy-fn more-preds args))))

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
