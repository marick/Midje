;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.tabular
  (:use 
    [clojure.string :only [join]]
    [midje.error-handling.monadic :only [error-let user-error-report-form validate]]
    [midje.internal-ideas.file-position :only [form-with-copied-line-numbers]]
    [midje.util.form-utils :only [ordered-zipmap translate pairs]]
    [midje.util.zip :only [skip-to-rightmost-leaf]]
    [midje.internal-ideas.expect :only [expect?]]
    [midje.ideas.arrows :only [above-arrow-sequence__add-key-value__at-arrow]])
(:require [midje.util.unify :as unify]))

(defn- binding-note [ordered-binding-map]
  (let [entries (map (fn [[variable value]] (str variable " " (pr-str value))) ordered-binding-map)]
    (str "{" (join ", " entries) "}")))

(defn add-one-binding-note [expect-containing-form ordered-binding-map]
  (translate expect-containing-form
    expect?
    (fn [loc] (skip-to-rightmost-leaf
      (above-arrow-sequence__add-key-value__at-arrow :binding-note (binding-note ordered-binding-map) loc)))))

(defn add-binding-notes [expect-containing-forms ordered-binding-maps]
  (map (partial apply add-one-binding-note) 
       (pairs expect-containing-forms ordered-binding-maps)))



(defn- remove-pipes+where [table]
  (let [strip-off-where #(if (contains? #{:where 'where} (first %)) (rest %) % )]
    (->> table strip-off-where (remove #(= % '|)))))	

(defn table-binding-maps [table]
  (let [[variables values] (split-with #(.startsWith (pr-str %) "?") (remove-pipes+where table))
        value-lists (partition (count variables) values)]
    (map (partial ordered-zipmap variables) value-lists)))




(defn- expander-for [fact-form]
  (comp macroexpand 
        #(form-with-copied-line-numbers % fact-form) 
        (partial unify/subst fact-form)))

(defn tabular* [forms]
  (error-let [[fact-form table] (validate forms)
              ordered-binding-maps (table-binding-maps table)
              expect-forms (map (expander-for fact-form)
                                ordered-binding-maps)
              result (add-binding-notes expect-forms ordered-binding-maps)]
    `(do ~@result)))

(defmethod validate "tabular" [form]
  (loop [forms (rest form)]
    (cond (string? (first forms))
          (recur (rest forms))

          (empty? (rest forms))
          (user-error-report-form
           form
           "There's no table. (Misparenthesized form?)")
          
          :else
          [ (first forms) (rest forms) ])))


