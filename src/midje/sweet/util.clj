(ns midje.sweet.util
  (:require [clojure.zip :as zip])
)

;; TODO: There must be a better way of handling namespaces.
(defn namespacey-match [symbols loc]
  (let [base-names (map name symbols)
	qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
				(map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str (zip/node loc)))))

