(ns ^{:doc "Recognizing Midje forms"}
  midje.parsing.util.recognizing
  (:require [clojure.zip :as zip]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.util.core :refer :all]
            [midje.parsing.util.future-variants :as future-variants]
            [midje.parsing.util.zip :as pzip]
            [midje.util.pile :as pile]))

;; Arrow groupings

(def expect-arrows #{=> =not=> =deny=> =future=> =expands-to=> =throw-parse-exception=>})
(def fake-arrows #{=> =contains=> =streams=> =throws=>})
(def all-arrows (clojure.set/union expect-arrows fake-arrows))

(defn mkfn:arrow? [& expected]
  (fn [actual] ((set expected) (name actual))))
(def common-check-arrow? (mkfn:arrow? => =not=> =deny=>))
(def macroexpansion-check-arrow? (mkfn:arrow? =expands-to=>))
(def future-check-arrow? (mkfn:arrow? =future=>))
(def parse-exception-arrow? (mkfn:arrow? =throw-parse-exception=>))


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
  (boolean (and (zip/right loc)
                (let [node (zip/node (zip/right loc))]
                  (and (symbol? node)
                       (expect-arrows (name node)))))))

(defmethod start-of-checking-arrow-sequence? :form [form]
  (boolean (and (sequential? form)
                (symbol? (second form))
                (expect-arrows (name (second form))))))

(defmulti start-of-prerequisite-arrow-sequence? tree-variant)

(defmethod start-of-prerequisite-arrow-sequence? :zipper [loc]
  (boolean (and (zip/right loc)
                (let [node (zip/node (zip/right loc))]
                  (and (symbol? node)
                       (fake-arrows (name node)))))))

(defmethod start-of-prerequisite-arrow-sequence? :form [form]
  (boolean (and (sequential? form)
                (symbol? (second form))
                (fake-arrows (name (second form))))))

(defn any-arrow? [loc]
  (boolean (and (symbol? (zip/node loc))
                (all-arrows (name (zip/node loc))))))


;;; Facts and what they contain

(defn fact? [form]
  (or (first-named? form "fact")
      (first-named? form "facts")
      (first-named? form "silent-fact"))) ;; silent facts are used for testing.

(defn future-fact? [form]
  (some (partial first-named? form) future-variants/future-fact-variant-names))

(defn tabular? [form]
  (first-named? form "tabular"))

(defn for-all? [form]
  (first-named? form "for-all"))

(defn provided? [loc]
  (boolean (and (symbol? (zip/node loc))
                (= "provided" (name (zip/node loc))))))

(defn metaconstant-prerequisite? [[lhs arrow rhs & overrides :as fake-body]]
  (symbol-named? arrow =contains=>))





(defmulti expect? tree-variant)

(defmethod expect? :zipper [loc]
  (and (zip/branch? loc)
       (expect? (zip/node loc))))

(defmethod expect? :form [form]
  (first-named? form "expect"))

(defn immediately-following-check-form? [loc]
  (expect? (pzip/previous-loc loc)))


(defn fake? [form]
  (or (first-named? form "fake")
      (first-named? form "data-fake")))
