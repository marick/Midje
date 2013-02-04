(ns midje.parsing.0-to-fact-form.t-tabular-method-size-explosion
  (:require [clojure.zip :as zip])
  (:use [midje sweet test-util]))

;;; This file demonstrates how easy it is for a tabular fact to explode into a huge method, yielding:
; Caused by: java.lang.ClassFormatError: Invalid method Code length
; 76145 in class file

(defn no-op [& args]
  `(:facts (do "something")))

(defn validation-error-form? [actual] true)

(tabular "method size EXPLOSION!"
  (future-facts "before, after and around validation"
    (fact "valid, then return rest of form"
      (no-op (cons ?wrapper `(:facts (do "something")))) => `(:facts (do "something")))
  
    (fact "wrapper's must use either :facts, :contents, or checks as their wrapping targets"
      (no-op (cons ?wrapper `(:abc (do "something")))) => validation-error-form?)
    
    (fact "correct form length" 
      (no-op (cons ?wrapper `(:facts (do "something") (do "another thing")))) => validation-error-form?
      (no-op (list ?wrapper)) => validation-error-form? ))

    ?wrapper
    'before 
    'after  
    'around)
