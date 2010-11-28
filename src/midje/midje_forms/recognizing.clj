(ns midje.midje-forms.recognizing
  (:use midje.util.form-utils)
  (:require [midje.util.wrapping :as wrapping])
  (:require [midje.util.unify :as unify])
  (:require [clojure.zip :as zip]))

;; TODO: Replace with form-first-like strategy?

(defn namespacey-match [symbols loc]
  (let [base-names (map name symbols)
	qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
				(map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str (zip/node loc)))))


(defn is-arrow-form? [forms]
  (= (str (second forms)) "=>"))

(defn fake? [form] (form-first? form "fake"))

;;; Wrapping

(def already-wrapped? wrapping/wrapped?)

(defn expect? [form] (form-first? form "expect"))
(def wrappable? expect?)

(defn fact-claim [form]
  (or (form-first? form "fact")
      (form-first? form "facts")))
(def expansion-has-wrappables? fact-claim)

(defn background-form? [form] (form-first? form "against-background"))
(def provides-wrappers? background-form?)

;;; background forms

(defn seq-headed-by-setup-teardown-form? [forms]
  (unify/bindings-map-or-nil (first forms) '(before :checking ?form)))
