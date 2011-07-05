;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.semi-sweet-errors
  (:use 
    [clojure.pprint :only [cl-format]]
    [midje.error-handling.monadic :only [user-error-report-form validate]]))

(defmethod validate "fake" [form]
  (cond (not (list? (second form)))
        (user-error-report-form
         form
         "The left-hand-side of a prerequisite must look like a function call."
         (cl-format nil "`~S` doesn't." (second form)))
        :else
        (rest form)))

(defmethod validate "expect" [form]
  (cond (< (count form) 4)
        (user-error-report-form form
         (cl-format nil "    This form: ~A" form)
         (cl-format nil "Doesn't match: (~A <actual> => <expected> [<keyword-value pairs>*])" (first form)))
        :else
        (rest form)))
