;; -*- indent-tabs-mode: nil -*-

(ns midje.tabular
  (:use [midje.error-handling monadic]
        [midje.midje-forms.translating :only [form-with-copied-line-numbers
                                              add-binding-notes]])
  (:use [midje.util.form-utils :only (ordered-zipmap)]
        [midje.util report file-position form-utils])
(:require [midje.util.unify :as unify]))

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


