;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.dissecting
  (:require [clojure.zip :as zip])
  (:require [midje.midje-forms.recognizing :as recognizing]))

(defn separate-background-forms [fact-forms]
  (let [background-forms (apply concat (map rest (filter recognizing/background-form?
                                                         fact-forms)))
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

(defn- remove-pipes [table]
  (remove #(= "|" (pr-str %)) table))

(defn- table-variables [table]
  (take-while #(.startsWith (pr-str %) "?") (remove-pipes table)))	

(defn- table-binding-maps [table]
  (let [variables (table-variables table)
        value-lists (rest (partition (count variables) (remove-pipes table)))]
    (map (partial zipmap variables) value-lists)))

(defn dissect-fact-table [forms]
  {:fact-form (first forms)
   :binding-maps (table-binding-maps (rest forms))
   :map-order (table-variables (rest forms))})


