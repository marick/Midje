;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.recognizing
  (:use
    [midje.util.namespace :only [namespacey-match]]
    [midje.background :only [background-form? setup-teardown-bindings]]
    [midje.checkers.defining :only [checker-makers checker?]]
    [midje.arrows :only [expect-arrows]]
    [midje.util.form-utils :only [form-first?]])
  (:require
    [clojure.zip :as zip]
    [midje.util.unify :as unify]
    [midje.util.wrapping :as wrapping]))



(defn is-head-of-form-providing-prerequisites? [loc]
  (namespacey-match '(provided) loc))
;;; background forms

(defn seq-headed-by-setup-teardown-form? [forms]
  (when-let [bindings (setup-teardown-bindings (first forms))]
    (and (bindings '?first-form)
         (or (not (bindings '?after)) (bindings '?second-form)))))

;; Folded prerequisites

;; Note that folded prerequisites are in semi-sweet-style. (That is, they can only
;; be recognized after sweet style has been converted to semi-sweet.)

(def special-forms '[quote fn let new])

(defn- constructor? [symbol]
  (.endsWith (name symbol) "."))

(defn- mockable-function-symbol? [symbol]
  (not (or (some #{symbol} special-forms)
           (some #{symbol} checker-makers)
           (constructor? symbol)
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

