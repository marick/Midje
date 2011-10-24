;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.arrows
  (:use midje.ideas.arrow-symbols)
  (:use [midje.util treelike namespace]
        [midje.util.form-utils :only (replace-matches)])
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

(defn- never->times-0
  "Replace syntactic sugar of `:never` with `:times 0`"
  [overrides]
  (replace-matches overrides #(= % :never ) `(:times 0)))

(def ^{:private true} length-of-override-key-val-pair 2)

(defn take-arrow-sequence-overrides [forms]
  "Extract key-value overrides from the arrow sequence;
   note: length taken can be one less than length of override,
         if :never was included in overrides"
  (let [overrides (->> forms
                       never->times-0
                       (partition length-of-override-key-val-pair)
                       (take-while (comp keyword? first))
                       (apply concat))
        length-taken (if (some #{:never } forms)
                         (- (count overrides) 1)
                         (count overrides))]
    [overrides length-taken]))

(def ^{:private true} length-of-constant-part 3)

(defn take-arrow-sequence [forms]
  "Extract the next arrow sequence from a longer sequence of forms."
  (let [constant-part (take length-of-constant-part forms)
        [overrides override-length-taken] (take-arrow-sequence-overrides (nthnext forms length-of-constant-part))
        arrow-seq (concat constant-part overrides)
        length-taken-to-read-arrow-seq (+ length-of-constant-part override-length-taken)]
    [arrow-seq length-taken-to-read-arrow-seq]))

(defn group-arrow-sequences
  ([fakes]
     (group-arrow-sequences [] fakes))
  ([so-far remainder]
    (if (empty? remainder)
      so-far
      (let [[arrow-seq length-taken] (take-arrow-sequence remainder)]
        (recur (conj so-far arrow-seq)
               (nthnext remainder length-taken))))))


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

