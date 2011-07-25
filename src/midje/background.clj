;; -*- indent-tabs-mode: nil -*-

(ns midje.background
  (:use
    [clojure.contrib.seq :only [separate]]
    [midje.util.form-utils :only [form-first?]])
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

