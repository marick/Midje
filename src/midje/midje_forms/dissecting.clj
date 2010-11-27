(ns midje.midje-forms.dissecting
  (:use [midje.midje-forms recognizing]))

(defn separate-background-forms [fact-forms]
  (let [background-forms (apply concat (map rest (filter background-form? fact-forms)))
	other-forms (filter (comp not background-form?) fact-forms)]
    [ background-forms other-forms ]))

(defn raw-wrappers [background-form]  (second background-form))

(defn interior-forms [form]
  `(do ~@(rest (rest form)))
  )

