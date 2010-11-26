(ns midje.util.transforming-midje-forms
  (:use [midje.util.recognizing-forms]))

(defn make-background [fake]
  (concat fake '(:type :background)))

(defn separate-fact [fact-forms]
  (let [background-forms (apply concat (map rest (filter background-form? fact-forms)))]
  [ background-forms
    (filter (comp not background-form?) fact-forms) ]))

