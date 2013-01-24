(ns ^{:doc "example maps, redefine maps, failure maps"} 
  midje.parsing.lexical-maps
  (:use [midje.util.form-utils :only [hash-map-duplicates-ok]])
  (:use midje.ideas.arrow-symbols
        midje.ideas.arrows
        [midje.util.form-utils :only [extended-fn?]])
  (:require [midje.internal-ideas.fact-context :as fact-context]
            [midje.parsing.util.fnref :as fnref]
            [midje.internal-ideas.file-position :as position])
  (:require [midje.parsing.3-from-lexical-maps.from-fake-maps :as from-fake-maps]))


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
    :arg-matchers (map from-fake-maps/mkfn:arg-matcher ~(vec args))
    :result-supplier (from-fake-maps/mkfn:result-supplier ~arrow ~result)
    :times :default  ; Default allows for a more attractive error in the most common case.

    :position (position/user-file-position)
    :call-count-atom (atom 0)
    :call-text-for-failures (str '~call-form)})


;;;                                             Metaconstant Detail Maps

;; A data fake is the implementation of `..metaconstant.. =contains=> {..}`.

(defmacro data-fake [metaconstant arrow contained]
  `{:type :fake
    :data-fake true
    :var ~(fnref/fnref-call-form metaconstant)
    :contained ~contained
    
    :position (position/user-file-position)
    :call-count-atom (atom 1) ;; kludje
    })
