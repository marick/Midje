(ns midje.util.forms
  (:require [clojure.zip :as zip]))

;; TODO: Replace with form-first-like strategy?

(defn namespacey-match [symbols loc]
  (let [base-names (map name symbols)
	qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
				(map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str (zip/node loc)))))


(defn form-first? [form desired]
  (and (list? form)
       (symbol? (first form))
       (= (name (first form)) desired)))

