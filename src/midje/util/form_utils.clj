;; -*- indent-tabs-mode: nil -*-

(ns midje.util.form-utils
   (:use [midje.util laziness]))

(defn regex? [thing]
  (= (class thing) java.util.regex.Pattern))

(defn symbol-named? [form desired]
  (and (symbol? form)
       (= (name form) desired)))

(defn form-first? [form desired]
  (and (sequential? form) (symbol-named? (first form) desired)))

(defn as-type [typed-sequential contents]
  (if (vector? typed-sequential)
    (vec contents)
    contents))

(defn separate-by [predicate forms]
  "Note that this is different than clojure.contrib.seq/separate." ;;HOW?
  (let [group (group-by predicate forms)]
    [ (group true) (group false) ]))

(defn reader-line-number [form]
  (or (:line (meta form))
      (some (comp :line meta) form)
      "0 (no line info)"))
  
(defn flatten-and-remove-nils [seq]
  (filter identity (flatten seq)))

(defn vector-without-element-at-index [index v]
  (vec (concat (subvec v 0 index) (subvec v (inc index)))))

(defn map-keys [function hashmap]
  "Return new map whose values are the the values
   of function applied to existing values."

  (into {} (map (fn [ [k v] ] [k (function v)])
                hashmap)))

(defn tack-on-to [hashmap & kvs]
  "conj new values onto appropriate keys of a map"
  (merge-with conj hashmap (apply (partial assoc {}) kvs)))

(defn pairs [first-seq second-seq]
  (partition 2 (interleave first-seq second-seq)))

(defn hash-map-duplicates-ok [& keys-and-vals]
  "Like hash-map, except duplicate keys are OK. Last one takes precedence."
  (if (empty? keys-and-vals)
    {}
    (apply (partial assoc {}) keys-and-vals)))

(defn apply-pairwise [ functions & arglists ]
  "(apply-pairwise [inc dec] [1 1] [2 2]) => [ [2 0] [3 1] ]
   Note that the functions must take only a single argument."
  (map (fn [arglist]
         (map (fn [function arg] (apply function [arg]))
              functions arglist))
       arglists))
