;; -*- indent-tabs-mode: nil -*-



(ns midje.expect
  (:use [midje.midje-forms.recognizing :only [loc-is-at-full-expect-form?]])
  (:require [clojure.zip :as zip]))

;; Moving around

(defn up-to-full-expect-form [loc]
  "From anywhere (recursively) within an expect form, move so that
   loc is at the full form (so that zip/down is 'expect)."
  (if (loc-is-at-full-expect-form? loc)
    loc
    (recur (zip/up loc))))

