;; -*- indent-tabs-mode: nil -*-

(ns midje.util.form-utils
  (:use [midje.util.treelike :only [tree-variant]]
        [clojure.set :only [difference]]
        [utilize.seq :only (first-truthy-fn)])
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

(defn vector-without-element-at-index [index v]
  (vec (concat (subvec v 0 index) (subvec v (inc index)))))

(defn pairs
  "Return [ (first first-seq) (first second-seq)] ..."
  [first-seq second-seq]
  (partition 2 (interleave first-seq second-seq)))

(defn hash-map-duplicates-ok
  "Like hash-map, except duplicate keys are OK. Last one takes precedence." 
  [& keys-and-vals]
  (if (empty? keys-and-vals)
    {}
    (apply assoc {} keys-and-vals)))     

(defn tack-on-to
  "Conj new values onto appropriate keys of a map" 
  [hashmap & kvs]
  (merge-with conj hashmap (apply hash-map-duplicates-ok kvs)))

(defn apply-pairwise
  "(apply-pairwise [inc dec] [1 1] [2 2]) => [ [2 0] [3 1] ]
   Note that the functions must take only a single argument." 
  [functions & arglists]
  (map (partial map 
  		(fn [f arg] (f arg)) 
  		functions) 
  	arglists))

(defn map-difference [bigger smaller]
  (select-keys bigger (difference (set (keys bigger)) (set (keys smaller)))))

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

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [x]
  (if (seq x)
    (map
     (fn [n _]
       (lazy-cat (drop n x) (take n x)))
     (iterate inc 0) x)
    (list nil)))

(defmacro pred-cond 
  "Checks each predicate against the item, returning the corresponding 
   result if it finds a match, otherwise returning nil.
   Assumes item to be a value, as it will get evaluated multiple times."
  [item pred result & preds+results]
  (cond (= pred :else ) result
        (not (seq preds+results)) `(if (~pred ~item) ~result nil) ;; last condition, but no :else in the form
        :else `(if (~pred ~item)
                 ~result
                 (pred-cond ~item ~@preds+results))))

(defmacro macro-for 
  "Macroexpands the body once for each of the elements in the 
   right-side argument of the bindings, which should be a seq"
  [bindings body] 
  `(let [macros# (for [~@bindings]
                     ~body)]
    `(do ~@macros#)))