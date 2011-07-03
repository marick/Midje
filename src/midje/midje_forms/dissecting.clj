;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.dissecting
  (:require [clojure.zip :as zip])
  (:use [midje.midje-forms.recognizing :only (background-form?)])
  (:use [clojure.contrib.seq-utils :only (separate)]))

;; dissecting background forms

(defn separate-background-forms [fact-forms]
  (let [[background-forms other-forms] (separate background-form? fact-forms)]
    [(mapcat rest background-forms) other-forms]))

(defn raw-wrappers [background-form] (second background-form))

(defn interior-forms [form]
  `(do ~@(rest (rest form))))

