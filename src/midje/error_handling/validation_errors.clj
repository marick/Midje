;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Code for identifying invalid Midje syntax. Includes control 
            flow macros, validation error creation, etc."}
  midje.error-handling.validation-errors
  (:use [clojure.algo.monads :only [defmonad domonad]]
        [clojure.test :only [report]]
        [midje.internal-ideas.file-position :only [form-position]]
        [midje.util.form-utils :only [named?]]
        [utilize.seq :only (find-first)]))
                           

;; Making validation errors

(defn- ^{:testable true } as-validation-error [form]
  (vary-meta form assoc :midje-syntax-validation-error true))

(defn validation-error-form? [form]
  (:midje-syntax-validation-error (meta form)))

(defn report-validation-error [form & notes]
  (as-validation-error `(report {:type :validation-error
                                 :notes '~notes
                                 :position '~(form-position form)})))

(defn simple-report-validation-error [form & notes]
  (apply report-validation-error form (conj (vec notes) (pr-str form))))


;; Validation control flow macros

(defmonad syntax-validate-m
  "Monad describing form processing with possible failures. Failure
  is represented by any form with metadata :midje-syntax-validation-error"
  [m-result identity
   m-bind   (fn [form f] 
              (if (validation-error-form? form) form (f form)))  ])

(defmacro when-valid [validatable-form & body-to-execute-if-valid]
  `(domonad syntax-validate-m [_# (validate ~validatable-form)]
     ~@body-to-execute-if-valid))


;; Validate

(defmulti validate (fn [form & options] 
                     (if (named? (first form)) 
                       (name (first form)) 
                       :validate-seq)))

(defn- ^{:testable true } spread-validation-error [collection]
  (or (find-first validation-error-form? collection)
    collection))

(defmethod validate :validate-seq [form & options] 
  (spread-validation-error (map validate form)))

(defmethod validate :default [form & options] (rest form))