;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.dissecting
  (:require [clojure.zip :as zip])
  (:require [midje.midje-forms.recognizing :as recognizing])
  (:use [midje.util.sequence :only (ordered-zipmap)]))

(defn separate-background-forms [fact-forms]
  (let [background-forms (mapcat rest (filter recognizing/background-form? fact-forms))
        other-forms (filter (comp not recognizing/background-form?) fact-forms)]
    [ background-forms other-forms ]))

(defn raw-wrappers [background-form]  (second background-form))

(defn interior-forms [form]
  `(do ~@(rest (rest form))))

(defn arrow-form-overrides [forms]
  "Extract key-value overrides from the sequence of forms"
  (apply concat (take-while (comp keyword? first) (partition 2 forms))))

(defn take-arrow-form [forms]
  "Extract the next fake from a sequence of forms."
  (let [constant-part (take 3 forms)
        overrides (arrow-form-overrides (nthnext forms 3))]
    (concat constant-part overrides)))

(defn partition-arrow-forms
  ([fakes]
     (partition-arrow-forms [] fakes))
  ([so-far remainder]
    (if (empty? remainder)
      so-far
      (let [whole-body (take-arrow-form remainder)]
        (recur (conj so-far whole-body)
               (nthnext remainder (count whole-body)))))))

;; dissecting tabular facts - could be in its own ns since it uses nothing from the above code
;; maybe midje.midje-forms.dissecting.tabular

(defn- remove-pipes+where [table]
  (let [strip-off-where #(if (contains? #{:where 'where} (first %)) (rest %) % )]
    (->> table strip-off-where (remove #(= "|" (pr-str %))))))	

(defn- table-binding-maps [table]
  (let [[variables values] (split-with #(.startsWith (pr-str %) "?") (remove-pipes+where table))
        value-lists (partition (count variables) values)]
    (map (partial ordered-zipmap variables) value-lists)))

(defn dissect-fact-table [[fact-form & table]]
  { :fact-form fact-form 
    :binding-maps (table-binding-maps table)})
