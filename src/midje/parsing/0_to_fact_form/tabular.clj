(ns ^{:doc "A way to create multiple facts with the same template, but different data points."}
  midje.parsing.0-to-fact-form.tabular
  (:require [clojure.string :as str]
            [clojure.zip :as zip]
            [midje.data.metaconstant :refer [metaconstant-symbol?]]
            [midje.emission.deprecation :refer [deprecate]]
            [midje.parsing.1-to-explicit-form.facts :as parse-facts]
            [midje.parsing.1-to-explicit-form.metadata :as metadata]
            [midje.parsing.lexical-maps :as maps]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.overrides :as override]
            [midje.parsing.util.zip :as pzip]
            [midje.parsing.util.zip :refer :all]
            [midje.parsing.util.zip :refer [skip-to-rightmost-leaf]]
            [midje.util.pile :as pile]
            [midje.util.unify :as unify]
            [pointer.core :refer [form-with-copied-line-numbers]]))

(defn- headings-rows+values [table locals]
  (letfn [(table-variable? [s]
            (and (symbol? s)
              (not (metaconstant-symbol? s))
              (not (resolve s))
              (not ((set locals) s))))]
    (split-with table-variable? table)))

(defn- ^{:testable true } table-binding-maps [headings-row values]
  (let [value-rows (partition (count headings-row) values)]
    (map (partial pile/ordered-zipmap headings-row) value-rows)))


(defn- valid-pieces [full-form locals]
  (let [[metadata [fact-form & table]] (metadata/separate-two-level-metadata full-form)
        [headings-row values] (headings-rows+values table locals)]
    (cond (empty? table)
          (error/report-error full-form
           "There's no table. (Misparenthesized form?)")

          (empty? values)
          (error/report-error full-form
            "It looks like the table has headings, but no values:")

          (empty? headings-row)
          (error/report-error full-form
            "It looks like the table has no headings, or perhaps you"
            "tried to use a non-literal string for the doc-string?:")

          :else
          [metadata fact-form headings-row values])))

(defn parse [locals form]
  (letfn [(macroexpander-for [fact-form]
            (fn [binding-map]
              (metadata/with-wrapped-metadata
                {:midje/table-bindings `(pile/ordered-zipmap '~(keys binding-map) '~(vals binding-map))}
                (parse-facts/working-on-nested-facts
                 (-> binding-map
                     ((partial unify/substitute fact-form))
                     ((partial form-with-copied-line-numbers fact-form))
                     macroexpand)))))]
    (error/parse-and-catch-failure form
      #(let [[metadata fact-form headings-row values] (valid-pieces form locals)
             ordered-binding-maps (table-binding-maps headings-row values)
             nested-facts (map (macroexpander-for fact-form) ordered-binding-maps)]
         (macroexpand (parse-facts/wrap-fact-around-body metadata nested-facts))))))
