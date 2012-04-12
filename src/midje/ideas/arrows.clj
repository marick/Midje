(ns ^{:doc "Arrows either indicate a form of expected result, or a stubbed prerequisite value."}
  midje.ideas.arrows
  (:use midje.ideas.arrow-symbols
        [clojure.set :only [union]]
        [midje.util form-utils treelike namespace])
  (:require [clojure.zip :as zip]))

;; Arrow groupings

(def expect-arrows #{=> =not=> =deny=> =future=> =expands-to=>})
(def fake-arrows #{=> =contains=> =streams=> =throws=>})
(def all-arrows (union expect-arrows fake-arrows))

(defn leaf-expect-arrows [nested-form]
  (let [named-form-leaves (map name (filter named? (flatten nested-form)))]
    (filter expect-arrows named-form-leaves)))

(defn leaves-contain-arrow? [nested-form]
  (not (empty? (leaf-expect-arrows nested-form))))

;; Recognizing

(defmulti start-of-checking-arrow-sequence? tree-variant)

(defmethod start-of-checking-arrow-sequence? :zipper [loc]
  (and (zip/right loc)
       (matches-symbols-in-semi-sweet-or-sweet-ns? expect-arrows (zip/right loc))))

(defmethod start-of-checking-arrow-sequence? :form [form]
  (and (sequential? form)
       (matches-symbols-in-semi-sweet-or-sweet-ns? expect-arrows (second form))))

;; Dissecting

(defn arrow-sequence-overrides
  "Extract key-value overrides from the arrow sequence"
  [forms]
  (apply concat (take-while (comp keyword? first) (partition 2 forms))))

(defn take-arrow-sequence
  "Extract the next arrow sequence from a longer sequence of forms."
  [forms]
  (let [constant-part (take 3 forms)
        overrides (arrow-sequence-overrides (nthnext forms 3))]
    (concat constant-part overrides)))

(defn pull-all-arrow-seqs-from
  ([fakes]
     (pull-all-arrow-seqs-from [] fakes))
  ([so-far remainder]
    (if (empty? remainder)
      so-far
      (let [arrow-seq (take-arrow-sequence remainder)]
        (recur (conj so-far arrow-seq)
               (nthnext remainder (count arrow-seq)))))))

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

