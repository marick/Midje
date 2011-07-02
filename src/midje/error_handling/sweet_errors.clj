;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.sweet-errors
  (:use [clojure.pprint :only [cl-format]]
        [midje.error-handling.monadic]
        [midje.util report file-position form-utils]))


(defmethod validate "tabular" [form]
  (loop [forms (rest form)]
    (cond (string? (first forms))
          (recur (rest forms))

          (empty? (rest forms))
          (user-error-report-form
           form
           "There's no table. (Misparenthesized form?)")
          
          :else
          [ (first forms) (rest forms) ])))


