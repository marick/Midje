(ns ^{:doc "Arrows either indicate a form of expected result, or a prerequisite value."}
  midje.parsing.util.arrows
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.arrow-symbols)
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


;; TODO: Old comment. This is a side-effect of having different sweet and semi-sweet
;; arrow symbols. Once semi-sweet is destroyed, this can be improved.

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn expect-match-or-mismatch [arrow]
  (condp = (name arrow) 
    => :expect-match
    =expands-to=> :expect-match
    =not=> :expect-mismatch
    =deny=> :expect-mismatch
    =test=> :just-midje-testing-here
    nil))

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

