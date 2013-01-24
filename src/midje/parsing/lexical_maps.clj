(ns ^{:doc "example maps, redefine maps, failure maps"} 
  midje.parsing.lexical-maps
  (:use [midje.util.form-utils :only [hash-map-duplicates-ok]])
  (:use midje.ideas.arrow-symbols
        midje.ideas.arrows
        [midje.util.form-utils :only [extended-fn?]])
  (:require [midje.internal-ideas.fact-context :as fact-context]
            [midje.parsing.util.fnref :as fnref]
            [midje.internal-ideas.file-position :as position]))


;;; Midje is driven off various kinds of maps. Those maps contain
;;; values that must capture the lexical context. For example, the
;;; right-hand-side of an arrow may be a symbol that's bound with a
;;; `let`. To capture the lexical context [*], we expand forms into
;;; "lexical maps". They are maps whose creators are expected to be
;;; macros. As a result, they are allowed to look like this:
;;;
;;;       :var (var foo)
;;;       :description (fact-context/nested-descriptions)
;;;       :checking-function (create-checker-from a)
;;;
;;; The right-hand sides are evaluated in the lexical environment
;;; of the original Midje facts. Therefore, for example, in the case
;;; of:
;;;
;;;     (let [a 1] (fact 1 => a))
;;;
;;; ... `create-checker-from` receives the value 1.
;;;
;;; The lexical maps are listed here so that there's something like a
;;; single point of reference for all the different "shapes" of
;;; maps. However, there is one way this reference is not definitive: 
;;; The parsing code that converts forms into template maps may add on keys
;;; that are useful for debugging or for tools that want to know where the
;;; final maps came from. To know what keys those are, see the relevant code.
;;; (The expectation is that such maps do not depend on the lexical environment.)

;;; I don't know if gathering all this in one place is really worth the trouble, but
;;; we'll see.

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


;;;                                             Fake Maps

;; A fake map describes all or part of a temporary rebinding of a var with a function that
;; captures invocations and also returns canned values.

(defmacro fake [call-form fnref args arrow result]
  ;; The (vec args) keeps something like (...o...) from being
  ;; evaluated as a function call later on. Right approach would
  ;; seem to be '~args. That causes spurious failures. Debug
  ;; someday.
  `{:type :fake
    :var ~(fnref/fnref-call-form fnref)
    :value-at-time-of-faking (if (bound? ~(fnref/fnref-call-form fnref))
                               ~(fnref/fnref-dereference-form fnref))
    :arg-matchers (map midje.internal-ideas.fakes/arg-matcher-maker ~(vec args))
    :result-supplier (midje.internal-ideas.fakes/fn-fake-result-supplier ~arrow ~result)

    :position (position/user-file-position)
    :call-count-atom (atom 0)
    :call-text-for-failures (str '~call-form)})


;;;                                             Metaconstant Detail Maps

;; An associative description map is a partial description of what a Metaconstant is
;; defined to contain.

;;;;; This should eventually be extracted from data-fake*

