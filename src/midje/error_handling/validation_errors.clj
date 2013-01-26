(ns ^{:doc "Code for identifying invalid Midje syntax. Includes control 
            flow macros, validation error creation, etc."}
  midje.error-handling.validation-errors
  (:use midje.clojure.core
        [clojure.algo.monads :only [defmonad domonad]]
        [midje.parsing.util.file-position :only [form-position]])
  (:require [midje.emission.api :as emit]))

                           

;; Making validation errors

(defn- ^{:testable true} as-validation-error [form]
  (vary-meta form assoc :midje/validation-error true))

(defn validation-error-form? [form]
  (:midje/validation-error (meta form)))

(defn validation-error-report-form [form & notes]
  (as-validation-error `(emit/fail {:type :parse-error
                                    :notes '~notes
                                    :position '~(form-position form)})))

(defn simple-validation-error-report-form [form & notes]
  (apply validation-error-report-form form (conj (vec notes) (pr-str form))))


;; Validation control flow macros

(defmonad validate-m
  "Monad describing form processing with possible failures. Failure
  is represented by any form with metadata :midje/validation-error"
  [m-result identity
   m-bind   (fn [form f] 
              (if (validation-error-form? form) form (f form)))  ])

(defmacro when-valid [validatable-form & body-to-execute-if-valid]
  `(domonad validate-m [_# (validate ~validatable-form)]
     ~@body-to-execute-if-valid))


;; Validate

(defmulti validate (fn [form & _options_] 
                     (if (named? (first form)) 
                       (name (first form)) 
                       :validate-seq)))

(defmethod validate :validate-seq [seq-of-forms & _options_]
  (let [first-validation-error (->> seq-of-forms 
                                    (map validate) 
                                    (filter validation-error-form?)
                                    first)]
    (or first-validation-error seq-of-forms)))

(defmethod validate :default [form & _options_] (rest form))
