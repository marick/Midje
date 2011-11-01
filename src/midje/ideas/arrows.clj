;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.arrows
  (:use midje.ideas.arrow-symbols
        [midje.util treelike namespace])
  (:require [clojure.zip :as zip]))

;; Arrow groupings
(def expect-arrows [=> =not=> =deny=> =future=> =expands-to=>])
(def fake-arrows [=> =contains=> =streams=>])
(def all-arrows (concat expect-arrows fake-arrows))


;; Recognizing

(defmulti is-start-of-checking-arrow-sequence? tree-variant)

(defmethod is-start-of-checking-arrow-sequence? :zipper [loc]
  (and (zip/right loc)
       (matches-symbols-in-semi-sweet-or-sweet-ns? expect-arrows (zip/right loc))))

(defmethod is-start-of-checking-arrow-sequence? :form [form]
  (and (sequential? form)
       (matches-symbols-in-semi-sweet-or-sweet-ns? expect-arrows (second form))))

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

(defn at-arrow__add-key-value-to-end__no-movement [key value loc]
  (-> loc
      zip/right
      (zip/insert-right value)
      (zip/insert-right key)
      zip/left))

(defn above-arrow-sequence__add-key-value__at-arrow [key value loc]
  (->> loc zip/down zip/right zip/right
       (at-arrow__add-key-value-to-end__no-movement key value)))

