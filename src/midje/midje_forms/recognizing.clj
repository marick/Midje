;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.recognizing
  (:use midje.util.form-utils
        [midje.checkers.defining :only [checker? checker-makers]]
        [midje.semi-sweet :only [expect-arrows]])
  (:require [midje.util.wrapping :as wrapping])
  (:require [midje.util.unify :as unify])
  (:require [clojure.zip :as zip]))

;; TODO: Make many of these multimethods, dispatching on whether "here"
;; is a clojure.zip loc?
;; TODO: Replace namespacey-match with form-first-like strategy?

(defn namespacey-match [symbols loc]
  (let [base-names (map name symbols)
        qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
                                (map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str (zip/node loc)))))


(defn is-arrow-form? [forms]
  (= (str (second forms)) "=>"))

(defn fake? [form] (form-first? form "fake"))

;; Clojure.zip trees

(defn loc-is-semi-sweet-keyword? [loc]
  (namespacey-match '(expect fake) loc))

(defn loc-is-head-of-form-providing-prerequisites? [loc]
  (namespacey-match '(provided) loc))

(defn loc-is-at-full-expect-form? [loc]
  (and (zip/branch? loc)
       (namespacey-match '(expect) (zip/down loc))))

;; A problem with not using namespacey-match for this is that
;; it will then match quoted Midje forms in a zillion tests.
(defn loc-is-start-of-check-sequence? [loc]
  (and (zip/right loc)
       (namespacey-match expect-arrows (zip/right loc))))

;; Wrapping

(def already-wrapped? wrapping/wrapped?)
(defn expect? [form] (form-first? form "expect"))
(defn background-form? [form] (form-first? form "against-background"))
(defn fact? [form]
  (or (form-first? form "fact")
      (form-first? form "facts")))
(defn future-fact? [form]
  (or (form-first? form "future-fact")
      (form-first? form "future-facts")
      (form-first? form "pending-fact")
      (form-first? form "pending-facts")
      (form-first? form "incipient-fact")
      (form-first? form "incipient-facts")
      (form-first? form "antiterminologicaldisintactitudinarian-fact")
      (form-first? form "antiterminologicaldisintactitudinarian-facts")))

;;; background forms

;; this actually should be in dissecting, but needs to be here to avoid
;; circularity. Feh.
(defn setup-teardown-bindings [form]
  (unify/bindings-map-or-nil form
                             '(?key ?when ?first-form ?after ?second-form)))

(defn seq-headed-by-setup-teardown-form? [forms]
  (when-let [bindings (setup-teardown-bindings (first forms))]
    (and (bindings '?first-form)
         (or (not (bindings '?after)) (bindings '?second-form)))))

;; Folded prerequisites

;; Note that folded prerequisites are in semi-sweet-style. (That is, they can only
;; be recognized after sweet style has been converted to semi-sweet.)

(def special-forms '[quote fn let])

(defn- mockable-function-symbol? [symbol]
  (not (or (some #{symbol} special-forms)
           (some #{symbol} checker-makers)
           (checker? (resolve symbol)))))

(defn mockable-funcall? [thing]
  (and (list? thing)
       (mockable-function-symbol? (first thing))))


;; It's annoying that these have to be here, instead of
;; dissecting, but I can't do circular references.
(defn fake-form-funcall [fake-form]
  (second fake-form))

(defn fake-form-funcall-arglist [fake-form]
  (rest (fake-form-funcall fake-form)))

(defn fake-that-needs-unfolding? [form]
  (and (sequential? form)
       (= 'midje.semi-sweet/fake (first form))
       ;; We now know this: (fake (f ...arg... ...arg...) ...)
       (some mockable-funcall? (fake-form-funcall-arglist form))))

