;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.moving-around
  (:use [midje.midje-forms.recognizing :only [loc-is-at-full-expect-form?]])
  (:require [clojure.zip :as zip]))

(defn up-to-full-expect-form [loc]
  "From anywhere (recursively) within an expect form, move so that
   loc is at the full form (so that zip/down is 'expect)."
  (if (loc-is-at-full-expect-form? loc)
    loc
    (recur (zip/up loc))))

(defn skip-to-rightmost-leaf [loc]
  "When positioned at leftmost position of branch, move to the end form.
   In a tree, that's the rightmost leaf."
  (let [end-form (zip/rightmost loc)]
    (if (zip/branch? end-form)
      (recur (zip/down end-form))
      end-form)))

(defn skip-down-then-rightmost-leaf [loc]
  "When positioned at a branch, move into it and then to the rightmost leaf."
  (skip-to-rightmost-leaf (zip/down loc)))

