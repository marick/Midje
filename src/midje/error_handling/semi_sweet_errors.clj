;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.semi-sweet-errors
  (:use [clojure.contrib.pprint :only [cl-format]]
        [midje.error-handling.monadic]
        [midje.util report file-position form-utils]))


(defmethod validate "fake" [form]
  (cond (not (list? (second form)))
        (user-error-report-form
         form
         (cl-format nil "Left-hand-side must look like a function call. `~S` doesn't."
                    (second form)))
        :else
        (rest form)))

(defmethod validate "expect" [form]
  (cond (< (count form) 4)
        (user-error-report-form form
         (cl-format nil "    This form: ~A" form)
         (cl-format nil "Doesn't match: (~A <actual> => <expected> [<keyword-value pairs>*])" (first form)))
        :else
        (rest form)))


