(ns ^{:doc "example maps, redefine maps, failure maps"} 
  midje.parsing.lexical-maps
  (:use midje.clojure.core
        midje.parsing.arrow-symbols
        midje.parsing.util.arrows)
  (:require [midje.data.nested-facts :as nested-facts]
            [midje.parsing.util.fnref :as fnref]
            [midje.parsing.util.file-position :as position])
  (:require [midje.parsing.3-from-lexical-maps.from-fake-maps :as from-fake-maps]))


;;; Because this code is a bit tricky, what with the lexical
;;; environment, each function follows a stylized form. If you want to see how the macro
;;; expands, uncomment the `pprint` of the results, and do something like this in the
;;; repl:
;;; 
;;; user=> (let [a 1]
;;;          (fact (cons a [2]) => (just a 2)))
;;;
;;; ... to see this:
;;;

(comment ; --------------------------------------------------------
  (clojure.core/merge
   {:position (midje.parsing.util.file-position/user-file-position),
    :expected-result-form '(just a 2),
    :expected-result (just a 2),
    :check-expectation :expect-match,
    :function-under-test (clojure.core/fn [] (cons a [2])),
    :description (midje.data.nested-facts/descriptions)}
   {:arrow '=>, :call-form '(cons a [2])}
   (midje.clojure.core/hash-map-duplicates-ok
    :position
    (midje.parsing.util.file-position/line-number-known 2)))
) ; ---------------------------------------------------------------

;;;                                             Example maps


(defmacro example
  [call-form arrow expected-result overrides]
  (let [source-details `{:call-form '~call-form
                         :arrow '~arrow }
        override-map `(hash-map-duplicates-ok ~@overrides)
        result `(merge
                 {:function-under-test (fn [] ~call-form)
                  :expected-result ~expected-result
                  :check-expectation ~(expect-match-or-mismatch arrow)
                  :expected-result-form '~expected-result ;; This is also part of the source details.
                  :position (position/user-file-position)
                  
                  ;; Adding this field insulates people writing emission plugins
                  ;; from the mechanism for keeping track of nested facts.
                  :description (nested-facts/descriptions)}
      
                 ~source-details
                 ~override-map)]
    ;; (pprint result)                     
    result))

;;;                                             Fake Maps

;; A fake map describes all or part of a temporary rebinding of a var with a function that
;; captures invocations and also returns canned values.

(defn fake [call-form fnref args arrow result overrides]
  (let [source-details `{:call-form '~call-form
                         :arrow '~arrow
                         :rhs '~(cons result overrides)}
        override-map `(hash-map-duplicates-ok ~@overrides)
        result `(merge
                 {
                  :type :fake
                  :var ~(fnref/fnref-call-form fnref)
                  :value-at-time-of-faking (if (bound? ~(fnref/fnref-call-form fnref))
                                             ~(fnref/fnref-dereference-form fnref))
                  :arg-matchers (map from-fake-maps/mkfn:arg-matcher ~(vec args))
                  :result-supplier (from-fake-maps/mkfn:result-supplier ~arrow ~result)
                  :times :default  ; Default allows for a more attractive error in the most common case.
                  
                  :position (position/user-file-position)
                  :call-count-atom (atom 0)
                  :call-text-for-failures (str '~call-form)
                 }
                 ~source-details
                 ~override-map)]
    ;; pprint result
    result))

;;;                                             Metaconstant Detail Maps

;; A data fake is the implementation of `..metaconstant.. =contains=> {..}`.

(defn data-fake [metaconstant arrow contained overrides]
  (let [source-details `{:call-form '~metaconstant
                         :arrow '~arrow
                         :rhs '~(cons contained overrides)}
        override-map `(hash-map-duplicates-ok ~@overrides)
        result `(merge
                 {
                  :type :fake
                  :data-fake true
                  :var ~(fnref/fnref-call-form metaconstant)
                  :contained ~contained
                  
                  :position (position/user-file-position)
                  :call-count-atom (atom 1) ;; kludje
                 }
                 ~source-details
                 ~override-map)]
    ;; pprint result
    result))

