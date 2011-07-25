;; -*- indent-tabs-mode: nil -*-
(ns midje.expect
  (:use [midje.util.treelike :only [tree-variant]]
        [midje.util.namespace :only [namespacey-match]]
        [midje.util.form-utils :only [form-first?]])
  (:require [clojure.zip :as zip]))
  

(defmulti expect? tree-variant)

(defmethod expect? :zipper [loc]
  (and (zip/branch? loc)
       (namespacey-match '(expect) (zip/down loc))))

(defmethod expect? :form [form]
  (form-first? form "expect"))




;; Moving around

(defn up-to-full-expect-form [loc]
  "From anywhere (recursively) within an expect form, move so that
   loc is at the full form (so that zip/down is 'expect)."
  (if (expect? loc)
    loc
    (recur (zip/up loc))))

