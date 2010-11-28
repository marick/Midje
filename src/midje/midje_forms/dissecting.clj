(ns midje.midje-forms.dissecting
  (:require [midje.midje-forms.recognizing :as recognizing]))

(defn separate-background-forms [fact-forms]
  (let [background-forms (apply concat (map rest (filter recognizing/background-form?
							 fact-forms)))
	other-forms (filter (comp not recognizing/background-form?) fact-forms)]
    [ background-forms other-forms ]))

(defn raw-wrappers [background-form]  (second background-form))

(defn interior-forms [form]
  `(do ~@(rest (rest form)))
  )
