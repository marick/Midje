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

