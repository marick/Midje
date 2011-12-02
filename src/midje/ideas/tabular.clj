;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.tabular
  (:use 
    [clojure.string :only [join]]
    [midje.error-handling.monadic :only [error-let user-error-report-form validate]]
    [midje.internal-ideas.file-position :only [form-with-copied-line-numbers
                                               form-position]] ; for deprecation
    [midje.util.report :only [midje-position-string]] ; for deprecation
    [midje.util.form-utils :only [ordered-zipmap translate-zipper]]
    [midje.util.zip :only [skip-to-rightmost-leaf]]
    [midje.internal-ideas.expect :only [expect?]]
    [midje.ideas.arrows :only [above-arrow-sequence__add-key-value__at-arrow]])
(:require [midje.util.unify :as unify]))

(defn- add-binding-note [expect-containing-form ordered-binding-map]
  (translate-zipper expect-containing-form
    expect?
    (fn [loc] (skip-to-rightmost-leaf
      (above-arrow-sequence__add-key-value__at-arrow :binding-note (pr-str ordered-binding-map) loc)))))

(def ^{:private true} deprecation-hack:file-position (atom ""))

(defn- remove-pipes+where [table]
  (when (and false (#{:where 'where} (first table)))
    (println "The `where` syntactic sugar for tabular facts is deprecated and will be removed in Midje 1.4." @deprecation-hack:file-position))

  (when (and false (some #(= % '|) table))
    (println "The `|` syntactic sugar for tabular facts is deprecated and will be removed in Midje 1.4." @deprecation-hack:file-position))
             
  (let [strip-off-where #(if (#{:where 'where} (first %)) (rest %) % )]
    (->> table strip-off-where (remove #(= % '|)))))

(defn- table-variable? [s] (.startsWith (pr-str s) "?"))

(defn- table-binding-maps [table]
  (let [[variables-row values] (split-with table-variable? (remove-pipes+where table))
        value-rows (partition (count variables-row) values)]
    (map (partial ordered-zipmap variables-row) value-rows)))

(defn- macroexpander-for [fact-form]
  (comp macroexpand
        (partial form-with-copied-line-numbers fact-form)
        (partial unify/subst fact-form)))

(defn tabular* [forms]
  (error-let [[fact-form table] (validate forms)
              _ (swap! deprecation-hack:file-position
                       (constantly (midje-position-string (form-position fact-form))))
              ordered-binding-maps (table-binding-maps table)
              expect-forms (map (macroexpander-for fact-form)
                                ordered-binding-maps)
              expect-forms-with-binding-notes (map add-binding-note
                                                   expect-forms
                                                   ordered-binding-maps)]
    `(do ~@expect-forms-with-binding-notes)))

(defmethod validate "tabular" [[_tabular_ & form]]
  (let [[fact-form & table] (drop-while string? form)]
    (if (empty? table)
      (user-error-report-form form "There's no table. (Misparenthesized form?)")
      [fact-form table])))
