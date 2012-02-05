;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Code for identifying invalid Midje syntax. Includes control 
            flow macros, validation error creation, etc."}
  midje.error-handling.validation-errors
  (:use
    [clojure.algo.monads :only [defmonad domonad with-monad m-lift]]
    [clojure.test :only [report]]
    [midje.internal-ideas.file-position :only [form-position]]
    [midje.util.form-utils :only [named?]]
    [utilize.seq :only (find-first)]))
                           

;; Making validation errors

(defn- #^:testable as-validation-error [form]
  (vary-meta form assoc :midje-validation-error true))

(defn validation-error-form? [form]
  (:midje-validation-error (meta form)))

(defn report-validation-error [form & notes]
  (as-validation-error `(report {:type :validation-error
                                 :notes '~notes
                                 :position '~(form-position form)})))

(defn simple-report-validation-error [form & notes]
  (apply report-validation-error form (conj (vec notes) (pr-str form))))


;; Special validation control flow macros

(defmonad midje-maybe-m
   "Monad describing form processing with possible failures. Failure
   is represented by any form with metadata :midje-validation-error"
   [m-result identity
    m-bind   (fn [mv f] (if (validation-error-form? mv) mv (f mv)))
    ])

(defmacro valid-let [let-vector & body]
  `(domonad midje-maybe-m ~let-vector ~@body))

(defn- #^:testable spread-validation-error [collection]
  (or (find-first validation-error-form? collection)
      collection))

;; This is a pretty dubious addition. Not using it now - found
;; a better way - but might need it later.
(defmacro with-valid [symbol & body]
  `(let [~symbol (#'spread-validation-error ~symbol)]
     (if (validation-error-form? ~symbol)
       (eval ~symbol)
       (do ~@body))))

(defmacro when-valid [validatable-form-or-forms & body-to-execute-if-valid]
  `(let [result# (validate ~validatable-form-or-forms)]
     (if (validation-error-form? result#)
       result#
       (do ~@body-to-execute-if-valid))))


;; Validate

(defmulti validate (fn [form & options] 
                     (if (named? (first form)) 
                       (name (first form)) 
                       :validate-seq)))

(defmethod validate :validate-seq [form & options] 
  (spread-validation-error (map validate form)))

(defmethod validate :default [form & options] (rest form))