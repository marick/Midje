(ns ^{:doc "Recognizing Midje forms"}
  midje.parsing.util.recognizing
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.arrow-symbols)
  (:require [clojure.zip :as zip]
            [midje.parsing.util.zip :as pzip]
            [midje.util.pile :as pile]
            [midje.parsing.util.future-variants :as future-variants]))

;; Arrow groupings

(def expect-arrows #{=> =not=> =deny=> =future=> =expands-to=> =throw-parse-exception=>})
(def fake-arrows #{=> =contains=> =streams=> =throws=>})
(def all-arrows (union expect-arrows fake-arrows))

(defn mkfn:arrow? [& expected]
  (fn [actual] ((set expected) (name actual))))
(def common-check-arrow? (mkfn:arrow? => =not=> =deny=>))
(def macroexpansion-check-arrow? (mkfn:arrow? =expands-to=>))
(def future-check-arrow? (mkfn:arrow? =future=>))
(def parse-exception-arrow? (mkfn:arrow? =throw-parse-exception=>))




;; TODO: Old comment. This is a side-effect of having different sweet and semi-sweet
;; arrow symbols. Once semi-sweet is destroyed, this can be improved.

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake
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



(defn start-of-prerequisite-arrow-sequence? [form]
  (and (sequential? form)
       (matches-symbols-in-semi-sweet-or-sweet-ns? fake-arrows (second form))))



;;; Backgrounds -- this is confusing while we're backing away from earlier,
;;; overly-baroque forms.

(defn against-background? [form]
  (or (first-named? form "against-background")
      (first-named? form "with-state-changes")))

(defn against-background-that-wraps? [form]
  (and (against-background? form)
       (and (> (count form) 2)
            (vector? (second form)))))

(defn against-background-that-applies-to-containing-fact? [form]
  (and (against-background? form)
       (not (against-background-that-wraps? form))))

(defn form-signaling-intention-to-wrap-background-around-fact? [form]
  (or (first-named? form "background")
      (first-named? form "prerequisites")
      (first-named? form "prerequisite")
      (against-background-that-applies-to-containing-fact? form)))


(defn first-form-could-be-a-code-runner? [forms]
  (and (or (list? (first forms))
           (seq? (first forms)))
       (symbol? (ffirst forms))))

(defn first-form-is-a-code-runner? [forms]
  (#{"before" "after" "around"} (name (ffirst forms))))


;;; Facts and what they contain

(defn fact? [form]
  (or (first-named? form "fact")
      (first-named? form "facts")
      (first-named? form "silent-fact"))) ;; silent facts are used for testing.

(defn future-fact? [form]
  (some (partial first-named? form) future-variants/future-fact-variant-names))

(defn provided? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(provided) loc))

(defn metaconstant-prerequisite? [[lhs arrow rhs & overrides :as fake-body]]
  (symbol-named? arrow =contains=>))





;;; The old "semi-sweet" forms linger on for a while longer.


(defmulti expect? tree-variant)

(defmethod expect? :zipper [loc]
  (and (zip/branch? loc)
       (matches-symbols-in-semi-sweet-or-sweet-ns? '(expect) (zip/down loc))))

(defmethod expect? :form [form]
  (first-named? form "expect"))

(defn immediately-following-check-form? [loc]
  (expect? (pzip/previous-loc loc)))


(defn fake? [form]
  (or (first-named? form "fake")
      (first-named? form "data-fake")))
