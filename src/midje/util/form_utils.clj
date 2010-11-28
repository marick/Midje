(ns midje.util.form-utils
   (:use [midje.util laziness]))

;; TODO: had to change list? to sequential? because unification produces lazyseqs.
(defn form-first? [form desired]
  (and (sequential? form)
       (symbol? (first form))
       (= (name (first form)) desired)))

(defn as-type [typed-sequential contents]
  (if (vector? typed-sequential)
    (vec contents)
    contents))

(defn separate-by [predicate forms]
  (let [group (group-by predicate forms)]
    [ (group true) (group false) ]))

