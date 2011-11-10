;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.semi-sweet-errors
  (:use 
    [clojure.pprint :only [cl-format]]
    [midje.error-handling.monadic :only [user-error-report-form validate]]
    [midje.util.namespace :only [matches-symbols-in-semi-sweet-or-sweet-ns?]]
    [midje.ideas.metaconstants :only [metaconstant-symbol?]]
    [midje.ideas.arrow-symbols :only [=contains=>]]))

(defn- compiler-will-inline-fn? [fn]
  (contains? (meta (resolve fn)) :inline))

(defmethod validate "fake" [[_fake_ & fake-form :as form]]
  (let [funcall (first fake-form)]
    (cond (not (list? funcall))
          (user-error-report-form
            form
            "The left-hand-side of a prerequisite must look like a function call."
            (cl-format nil "`~S` doesn't." funcall))

          (compiler-will-inline-fn? (first funcall))
          (user-error-report-form
           form
           (cl-format nil "You cannot override the function `~S`: it is inlined by the Clojure compiler." (first funcall)))

          
          :else
          fake-form)))

(defmethod validate "data-fake" [[header metaconstant arrow hash & remainder :as form]]
  (cond (not (metaconstant-symbol? metaconstant))
        (user-error-report-form
          form
          "You seem to be assigning values to a metaconstant, but there's no metaconstant.")

        (not= (matches-symbols-in-semi-sweet-or-sweet-ns? '(arrow) =contains=>))
        (user-error-report-form
          form
          "Assigning values to a metaconstant requires =contains=>")

        :else
        (rest form)))

(defmethod validate "expect" [form]
  (if (< (count form) 4)
    (user-error-report-form form
      (cl-format nil "    This form: ~A" form)
      (cl-format nil "Doesn't match: (~A <actual> => <expected> [<keyword-value pairs>*])" (first form)))
    (rest form)))
