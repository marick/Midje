;; -*- indent-tabs-mode: nil -*-

(ns midje.util.form-utils
  (:use [midje.util.treelike :only [tree-variant]]
        [clojure.set :only [difference]]
        [ordered.map :only [ordered-map]])
  (:require [clojure.zip :as zip]))

(defn regex? [thing]
  (= (class thing) java.util.regex.Pattern))

(defn classic-map? [x] (.isInstance clojure.lang.APersistentMap x))
  
(defn record? [x]  (and (map? x) (not (classic-map? x))))
  
(defn symbol-named?
  "Is the thing a symbol with the name given by the string?"
  [thing string]
  (and (symbol? thing)
       (= (name thing) string)))

(defn form-first?
  "Is the form's first element a symbol whose name is the desired string?"
  [form desired]
  (and (sequential? form) (symbol-named? (first form) desired)))


(defmulti quoted? tree-variant)
(defmethod quoted? :zipper [loc]
  (quoted? (zip/node loc)))
(defmethod quoted? :form [form]
  (form-first? form "quote"))

(defn preserve-type
  "If the original form was a vector, make the transformed form a vector too."
  [original-form transformed-form]
  (if (vector? original-form)
    (vec transformed-form)
    transformed-form))

(defn separate-by
  "Like clojure.core/separate, but not lazy, returns nil for empy list."
  ;; TODO: One of those two differences is the difference between
  ;; a blowup in PersistentArrayMap and no blowup. Should investigate.
  [predicate forms]
  (let [group (group-by predicate forms)]
    [ (group true) (group false) ]))

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
  "conj new values onto appropriate keys of a map"
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
  "like zipmap, but guarantees order of the entries"
  (loop [m (ordered-map)
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs)) 
             (next ks)
             (next vs))
      m)))

(defn first-true [[pred & more-preds] & args]	
  (when pred
    (if (apply pred args)
        pred
        (apply first-true more-preds args))))

;; traverses the zipper; for the first (only the first!) predicate matching a 
;; node, calls the corresponding translate function. Then, continues traversing.   
(defn translate [form & preds+translate-fns]
  (loop [loc (zip/seq-zip form)]
      (if (zip/end? loc)
          (zip/root loc)
          (if-let [true-fn (first-true (map first (partition 2 preds+translate-fns)) loc)]
            (recur (zip/next ((get (apply hash-map preds+translate-fns) true-fn) loc)))
            (recur (zip/next loc))))))
