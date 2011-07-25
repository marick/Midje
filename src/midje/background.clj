;; -*- indent-tabs-mode: nil -*-

(ns midje.background
  (:use
    [clojure.contrib.seq :only [separate]]
    [midje.util.form-utils :only [form-first?]]
    [midje.util.wrapping :only [ensure-correct-form-variable]])
  (:require [midje.util.unify :as unify :only [bindings-map-or-nil]]))

(defn background-form? [form] (form-first? form "against-background"))


;; dissecting background forms

(defn separate-background-forms [fact-forms]
  (let [[background-forms other-forms] (separate background-form? fact-forms)]
    [(mapcat rest background-forms) other-forms]))

(defn raw-wrappers [background-form] (second background-form))

(defn setup-teardown-bindings [form]
  (unify/bindings-map-or-nil form
                             '(?key ?when ?first-form ?after ?second-form)))


(defmacro before [wrapping-target before-form & [_ after-form & _ ] ]
  (ensure-correct-form-variable `(try
                                  ~before-form
                                  ?form
                                  (finally ~after-form))))

(defmacro after [wrapping-target after-form]
  (ensure-correct-form-variable `(try ?form (finally ~after-form))))

(defmacro around [wrapping-target around-form]
  (ensure-correct-form-variable around-form))


