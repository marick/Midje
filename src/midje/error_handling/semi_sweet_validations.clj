(ns ^{:doc "Validation methods confirming the proper syntax of semi-sweet macros."}
  midje.error-handling.semi-sweet-validations
  (:use [clojure.pprint :only [cl-format]]
        [midje.error-handling.validation-errors :only [validation-error-report-form validate]]
        [midje.util.namespace :only [matches-symbols-in-semi-sweet-or-sweet-ns?]]
        [midje.data.metaconstant :only [metaconstant-symbol?]]
        [midje.parsing.arrow-symbols :only [=contains=>]])
  (:require [midje.parsing.util.fnref :as fnref]))

(letfn [(compiler-will-inline-fn? [fnref]
          (contains? (meta (fnref/fnref-var-object fnref)) :inline))
        (exposed-testable? [fnref]
          (contains? (meta (fnref/fnref-var-object fnref)) :testable))]

  (defmethod validate "fake" [[_fake_ & fake-form :as form]]
    (let [funcall (first fake-form)]
      (cond (not (list? funcall))
        (validation-error-report-form
          form
          "The left-hand-side of a prerequisite must look like a function call."
          (cl-format nil "`~S` doesn't." funcall))

        (compiler-will-inline-fn? (first funcall))
        (validation-error-report-form
          form
          (cl-format nil "You cannot override the function `~S`: it is inlined by the Clojure compiler." (first funcall)))

        (exposed-testable? (first funcall))
        (validation-error-report-form
          form
          "A prerequisite cannot use a symbol exposed via `expose-testables` or `testable-privates`."
          (cl-format nil "Instead, use the var directly: #'~S/~S"
                     (-> (first funcall) fnref/fnref-var-object meta :ns ns-name)
                     (first funcall)))

        :else
        form))))

(defmethod validate "data-fake" [[_data-fake_ metaconstant arrow hash & remainder :as form]]
  (cond (not (metaconstant-symbol? metaconstant))
        (validation-error-report-form
          form
          "You seem to be assigning values to a metaconstant, but there's no metaconstant.")

        (not= (matches-symbols-in-semi-sweet-or-sweet-ns? '(arrow) =contains=>))
        (validation-error-report-form
          form
          "Assigning values to a metaconstant requires =contains=>")

        :else
        form))

(defmethod validate "expect" [form]
  (if (< (count form) 4)
    (validation-error-report-form form
      (cl-format nil "    This form: ~A" form)
      (cl-format nil "Doesn't match: (~A <actual> => <expected> [<keyword-value pairs>*])" (first form)))
    (rest form)))
