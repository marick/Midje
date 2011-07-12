;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.dissecting
  (:use
    [clojure.contrib.seq :only [separate]]
    [midje.midje-forms.recognizing :only [background-form?]]))

;; dissecting background forms

(defn separate-background-forms [fact-forms]
  (let [[background-forms other-forms] (separate background-form? fact-forms)]
    [(mapcat rest background-forms) other-forms]))

(defn raw-wrappers [background-form] (second background-form))

(defn interior-forms [form]
  `(do ~@(rest (rest form))))

