(ns ^{:doc "The maps everything compiles down to."} 
  midje.data.core-maps
  (:use [midje.util.form-utils :only [hash-map-duplicates-ok]])
  (:use midje.ideas.arrow-symbols)
  (:require [midje.internal-ideas.fact-context :as fact-context]
            [midje.internal-ideas.file-position :as position]))

;; TODO: Old comment. What's up with this?

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn check-for-arrow [arrow]
  (condp = (name arrow) 
    => :check-match
    =expands-to=> :check-match
    =not=> :check-negated-match
    =deny=> :check-negated-match
    =test=> :just-midje-testing-here
    nil))

(defmacro make-example-map
  "This the map created from an arrow form representing an example of
   a containing fact."
  [call-form arrow expected-result overrides]
  `(merge
    {:description (fact-context/nested-descriptions)
     :function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :desired-check ~(check-for-arrow arrow)
     :expected-result-form '~expected-result 
     :position (position/user-file-position)
     
     ;; for Midje tool creators:
     :call-form '~call-form
     :arrow '~arrow }
     (hash-map-duplicates-ok ~@overrides)))
