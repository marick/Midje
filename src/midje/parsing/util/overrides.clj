(ns ^{:doc "The idea of key-value overrides is common to various forms"}
  midje.parsing.util.overrides
  (:require [clojure.zip :as zip]))

(defn arrow-sequence-overrides
  "Extract key-value overrides from the arrow sequence"
  [forms]
  (apply concat (take-while (comp keyword? first) (partition 2 forms))))

(defn at-arrow__add-key-value-to-end__no-movement [key value loc]
  (-> loc
      zip/right
      (zip/insert-right value)
      (zip/insert-right key)
      zip/left))

(defn above-arrow-sequence__add-key-value__at-arrow [key value loc]
  (->> loc zip/down zip/right zip/right
       (at-arrow__add-key-value-to-end__no-movement key value)))

