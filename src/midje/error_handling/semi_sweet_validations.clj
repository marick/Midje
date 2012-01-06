;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.semi-sweet-validations
  (:use 
    [clojure.pprint :only [cl-format]]
    [midje.error-handling.validation-errors :only [report-validation-error validate]]
    [midje.util.namespace :only [matches-symbols-in-semi-sweet-or-sweet-ns?]]
    [midje.ideas.metaconstants :only [metaconstant-symbol?]]
    [midje.ideas.arrow-symbols :only [=contains=>]]))

(defn- compiler-will-inline-fn? [fn]
  (contains? (meta (resolve fn)) :inline))

(defmethod validate "fake" [[_fake_ & fake-form :as form]]
  (let [funcall (first fake-form)]
    (cond (not (list? funcall))
          (report-validation-error
            form
            "The left-hand-side of a prerequisite must look like a function call."
            (cl-format nil "`~S` doesn't." funcall))

          (compiler-will-inline-fn? (first funcall))
          (report-validation-error
           form
           (cl-format nil "You cannot override the function `~S`: it is inlined by the Clojure compiler." (first funcall)))

          
          :else
          fake-form)))

(defmethod validate "data-fake" [[header metaconstant arrow hash & remainder :as form]]
  (cond (not (metaconstant-symbol? metaconstant))
        (report-validation-error
          form
          "You seem to be assigning values to a metaconstant, but there's no metaconstant.")

        (not= (matches-symbols-in-semi-sweet-or-sweet-ns? '(arrow) =contains=>))
        (report-validation-error
          form
          "Assigning values to a metaconstant requires =contains=>")

        :else
        (rest form)))

(defmethod validate "expect" [form]
  (if (< (count form) 4)
    (report-validation-error form
      (cl-format nil "    This form: ~A" form)
      (cl-format nil "Doesn't match: (~A <actual> => <expected> [<keyword-value pairs>*])" (first form)))
    (rest form)))
