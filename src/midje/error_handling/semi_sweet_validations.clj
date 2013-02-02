(ns ^{:doc "Validation methods confirming the proper syntax of semi-sweet macros."}
  midje.error-handling.semi-sweet-validations
  (:use midje.clojure.core
        midje.parsing.util.core
        [midje.error-handling.validation-errors :only [validation-error-report-form validate]]
        [midje.data.metaconstant :only [metaconstant-symbol?]]
        [midje.parsing.arrow-symbols :only [=contains=>]])
  (:require [midje.parsing.util.fnref :as fnref]))

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

