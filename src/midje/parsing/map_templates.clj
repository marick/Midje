(ns ^{:doc "example maps, redefine maps, failure maps"} 
  midje.parsing.map-templates
  (:use [midje.util.form-utils :only [hash-map-duplicates-ok]])
  (:use midje.ideas.arrow-symbols
        midje.ideas.arrows
        [midje.util.form-utils :only [extended-fn?]])
  (:require [midje.internal-ideas.fact-context :as fact-context]
            [midje.internal-ideas.file-position :as position]))


;;; Midje is driven off various kinds of maps. These are created via
;;; macroexpansion into "template maps" that associate keywords
;;; together with not-yet-evaluated forms that produce values.  Some
;;; of the forms are self-evaluating literals. For example, arrows
;;; might be transformed into keywords so that the map contains this:
;;;
;;;       :check-expectation :expect-match
;;;
;;; But more common are form like:
;;;
;;;       :var (var foo)
;;;       :description (fact-context/nested-descriptions)
;;;
;;; When the result of the macroexpansion is evaluated, the final maps are created.
;;; All this rigamarole is required so that the final maps have access to the lexical
;;; environment.
;;;
;;; The template maps are listed here so that there's something like a
;;; single point of reference for all the different "shapes" of
;;; maps. However, there is one way this reference is not definitive: 
;;; The parsing code that converts forms into template maps may add on keys
;;; that are useful for debugging or for tools that want to know where the
;;; final maps came from. To know what keys those are, see the relevant code. 

;;;                                             Example maps


(defmacro example
  [call-form arrow expected-result overrides]
  `(merge
    {:description (fact-context/nested-descriptions)
     :function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :check-expectation ~(expect-match-or-mismatch arrow)
     :expected-result-form '~expected-result 
     :position (position/user-file-position)
     
     ;; for Midje tool creators:
     :call-form '~call-form
     :arrow '~arrow }
    
    (hash-map-duplicates-ok ~@overrides)))


;;;                                             Redefine Maps

;; A redefine map describes all or part of a future redefinition of a var. Redefine are used
;; to produce mock/fake style functions that record their invocations and also return canned
;; values.

;;;;; This should eventually be extracted from fake*


;;;                                             Metaconstant Detail Maps

;; An associative description map is a partial description of what a Metaconstant is
;; defined to contain.

;;;;; This should eventually be extracted from data-fake*

