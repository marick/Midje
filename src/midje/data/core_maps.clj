(ns ^{:doc "example maps, redefine maps, failure maps"} 
  midje.data.core-maps
  (:use [midje.util.form-utils :only [hash-map-duplicates-ok]])
  (:use midje.ideas.arrow-symbols
        [midje.util.form-utils :only [extended-fn?]])
  (:require [midje.internal-ideas.fact-context :as fact-context]
            [midje.internal-ideas.file-position :as position]))


;;;                                             Example maps


;; TODO: Old comment. This is a side-effect of having different sweet and semi-sweet
;; arrow symbols. Once semi-sweet is destroyed, this can be improved.

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn expect-match-or-mismatch [arrow]
  (condp = (name arrow) 
    => :expect-match
    =expands-to=> :expect-match
    =not=> :expect-mismatch
    =deny=> :expect-mismatch
    =test=> :just-midje-testing-here
    nil))

(defmacro make-example-map
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

(def has-function-checker? (comp extended-fn? :expected-result))


;;;                                             Redefine Maps

;; A redefine map describes all or part of a future redefinition of a var. Redefine are used
;; to produce mock/fake style functions that record their invocations and also return canned
;; values.

;;;;; This should eventually be extracted from fake*


;;;                                             Metaconstant Detail Maps

;; An associative description map is a partial description of what a Metaconstant is
;; defined to contain.

;;;;; This should eventually be extracted from data-fake*


;;;                                             Failure maps

;; A failure map is typically constructed by adding key/value pairs to existing maps.

(defn minimal-failure-map [type actual existing]
  (assoc existing :type type :actual actual))


