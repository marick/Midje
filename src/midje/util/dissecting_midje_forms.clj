(ns midje.util.dissecting-midje-forms
  (:use [midje.util recognizing-midje-forms]))

(defn separate-background-forms [fact-forms]
  (let [background-forms (apply concat (map rest (filter background-form? fact-forms)))
	other-forms (filter (comp not background-form?) fact-forms)]
    [ background-forms other-forms ]))


