;; -*- indent-tabs-mode: nil -*-

(ns midje.arrows
  (:use midje.arrow-symbols)
  (:use [midje.util treelike namespace])
  (:use [midje.util.file-position :only [line-number-known]])
  (:require [clojure.zip :as zip]))

;; Arrow groupings
(def expect-arrows [=> =not=> =deny=> =future=>])
(def fake-arrows [=> =streams=>])
(def all-arrows (concat expect-arrows fake-arrows))


;; Recognizing

(defmulti is-start-of-arrow-sequence? tree-variant)
(defmethod is-start-of-arrow-sequence? :zipper [loc]
  (and (zip/right loc)
       (namespacey-match expect-arrows (zip/right loc))))
(defmethod is-start-of-arrow-sequence? :form [form]
  (and (sequential? form)
       (namespacey-match expect-arrows (second form))))

;; Dissecting

(defn arrow-sequence-overrides [forms]
  "Extract key-value overrides from the arrow sequence"
  (apply concat (take-while (comp keyword? first) (partition 2 forms))))

(defn take-arrow-sequence [forms]
  "Extract the next arrow sequence from a longer sequence of forms."
  (let [constant-part (take 3 forms)
        overrides (arrow-sequence-overrides (nthnext forms 3))]
    (concat constant-part overrides)))

(defn group-arrow-sequences
  ([fakes]
     (group-arrow-sequences [] fakes))
  ([so-far remainder]
    (if (empty? remainder)
      so-far
      (let [whole-body (take-arrow-sequence remainder)]
        (recur (conj so-far whole-body)
               (nthnext remainder (count whole-body)))))))

;; Editing

(defn at_arrow__add-key-value-to-end__no-movement [key value loc]
  (-> loc
      zip/right
      (zip/insert-right value)
      (zip/insert-right key)
      zip/left))

(defn above_arrow_sequence__add-key-value__at_arrow [key value loc]
  (->> loc zip/down zip/right zip/right
       (at_arrow__add-key-value-to-end__no-movement key value)))

(defn at_arrow__add-line-number-to-end__no-movement [number loc]
  (at_arrow__add-key-value-to-end__no-movement
   :position `(line-number-known ~number) loc))
