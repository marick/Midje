(ns midje.error-handling.t-background-validations
  (:require [clojure.zip :as zip])
  (:use [midje sweet test-util]))

(prn "T_BACKGROUND_VALIDATIONS")
;; (tabular
;;   (facts "before, after and around validation"
;;     (fact "valid, then return rest of form"
;;       (validate (cons ?wrapper `(:facts (do "something")))) => `(:facts (do "something")))
  
;;     (fact "wrapper's must use either :facts, :contents, or checks as their wrapping targets"
;;       (validate (cons ?wrapper `(:abc (do "something")))) => validation-error-form?)
    
;;     (fact "correct form length" 
;;       (validate (cons ?wrapper `(:facts (do "something") (do "another thing")))) => validation-error-form?
;;       (validate (list ?wrapper)) => validation-error-form? ))

;;     ?wrapper
;;     'before 
;;     'after  

;;     'around)
