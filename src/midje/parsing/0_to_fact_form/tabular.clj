(ns ^{:doc "A way to create multiple facts with the same template, but different data points."}
  midje.parsing.0-to-fact-form.tabular
  (:use midje.clojure.core
        midje.parsing.util.zip
        [midje.parsing.util.file-position :only [form-with-copied-line-numbers]]
        [midje.emission.deprecation :only [deprecate]]
        [midje.parsing.util.zip :only [skip-to-rightmost-leaf]]
        [midje.parsing.1-to-explicit-form.facts :only [working-on-nested-facts unparse-edited-fact]]
        [midje.data.metaconstant :only [metaconstant-symbol?]]
        [utilize.map :only [ordered-zipmap]])
(:require [clojure.string :as str]
          [clojure.zip :as zip]
          [midje.util.unify :as unify]
          [midje.parsing.util.overrides :as override]
          [midje.parsing.lexical-maps :as maps]
          [midje.parsing.1-to-explicit-form.metadata :as metadata]
          [midje.parsing.util.error-handling :as error]))

(defn- remove-pipes+where [table]
  (when (#{:where 'where} (first table))
    (deprecate "The `where` syntactic sugar for tabular facts is deprecated and will be removed in Midje 1.6."))

  (when (some #(= % '|) table)
    (deprecate "The `|` syntactic sugar for tabular facts is deprecated and will be removed in Midje 1.6."))

  (letfn [(strip-off-where [x] (if (#{:where 'where} (first x)) (rest x) x))]
    (->> table strip-off-where (remove #(= % '|)))))

(defn- headings-rows+values [table locals]
  (letfn [(table-variable? [s]
            (and (symbol? s)
              (not (metaconstant-symbol? s))
              (not (resolve s))
              (not ((set locals) s))))] 
    (split-with table-variable? (remove-pipes+where table))))

(defn- ^{:testable true } table-binding-maps [headings-row values]
  (let [value-rows (partition (count headings-row) values)]
    (map (partial ordered-zipmap headings-row) value-rows)))

(defn- format-binding-map [binding-map] 
  (let [formatted-entries (for [[k v] binding-map]
                            (str (pr-str k) " " (pr-str v)))]
    (str "[" (str/join "\n                           " formatted-entries) "]")))

(defn- ^{:testable true } add-binding-note
  [checking-fact-form ordered-binding-map]
  ;; A binding note should be added if the structure of the
  ;; `checking-fact-form` is this:
  ;;    (creation-time-check
  ;;      (letfn [...] <letfn-body>
  ;;
  ;; It is the <letfn-body> that must be searched for the construction of checkable maps,
  ;; which then have annotations added to them.
  (letfn [(headed-by? [form string]
            (and (sequential? form)
                 (symbol? (first form))
                 (= (name (first form)) string)))

          (acceptable-body? []
            (headed-by? checking-fact-form "creation-time-check"))

          (target-body []
            ;; This is horrible. `tabular` can be wrapped around two different
            ;; trees, depending on whether there's a wrapping `against-background`.
            ;; This handles that. I am ashamed of the losingness that is against-background. 
            (let [possible-letfn (second checking-fact-form)
                  definite-letfn (if (headed-by? possible-letfn "letfn")
                                   possible-letfn
                                   (second possible-letfn))]
            (-> definite-letfn second first rest second)))

          (translate-letfn-body [checkable-containing-form]
            ;; TODO: Nested facts lead to nested letfns, and that stops processing,
            ;; so nested facts don't get binding annotations.
            (translate-zipper checkable-containing-form
                              (comp maps/checkable-map? zip/node)
                              one-binding-note))

          (one-binding-note [loc]
            (zip/replace loc
                         (assoc (clojure.zip/node loc) 
                                :binding-note (format-binding-map ordered-binding-map))))]

    (if (acceptable-body?)
      (let [letfn-body (target-body)]
        (clojure.walk/prewalk-replace {letfn-body (translate-letfn-body letfn-body)}
                                      checking-fact-form))
      checking-fact-form)))

(defn valid-pieces [full-form locals]
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
              (working-on-nested-facts
               (-> binding-map
                   ((partial unify/substitute fact-form))
                   ((partial form-with-copied-line-numbers fact-form))
                  macroexpand))))]
    (error/parse-and-catch-failure form
      #(let [[metadata fact-form headings-row values] (valid-pieces form locals)
             ordered-binding-maps (table-binding-maps headings-row values)
             nested-facts (map (macroexpander-for fact-form) ordered-binding-maps)
             nested-facts-with-binding-notes (map add-binding-note
                                                  nested-facts
                                                  ordered-binding-maps)]
         (macroexpand (unparse-edited-fact metadata nested-facts-with-binding-notes))))))
