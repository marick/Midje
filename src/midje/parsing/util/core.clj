(ns ^{:doc "Utility functions dealing with checking or tranforming forms or zippers."}
  midje.parsing.util.core
  (:require [clojure.zip :as zip]))



(defn tree-variant [treelike]
  (letfn [(is-zipper? [treelike]
            (:zip/make-node (meta treelike)))]
    (if (is-zipper? treelike) :zipper :form)))

(defn symbol-named?
  "Is the thing a symbol with the name given by the string?"
  [x string]
  (and (symbol? x)
       (= (name x) string)))

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

(def dequote #(if (quoted? %) (second %) %))


(defn reader-list-form?
  "True if the form is a parenthesized list of the sort the reader can return."
  [form]
  (or (list? form) (= (type form) clojure.lang.Cons)))

(defn quoted-list-form?
  "True if the form is a quoted list such as the reader might return"
  [form]
  (and (reader-list-form? form)
       (quoted? form)))


(defn preserve-type
  "If the original form was a vector, make the transformed form a vector too."
  [original-form transformed-form]
  (if (vector? original-form)
    (vec transformed-form)
    transformed-form))

(defn reader-line-number
  "Find what line number the reader put on the given form or on
   one of its elements. If no line numbers, a warning string."
  [form]
  (or (:line (meta form))
      (some (comp :line meta) form)
      "0 (no line info)"))

(defn arglist-undoing-nesting [args]
  (if (and (= 1 (count args))
           (vector? (first args)))
    (first args)
    args))

