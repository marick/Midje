(ns midje.parsing.lexical-maps
  "checkable maps, redefine maps, failure maps"
  (:require [such.shorthand :as shorthand]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.3-from-lexical-maps.from-fake-maps :as from-fake-maps]
            [midje.parsing.util.fnref :as fnref]
            [midje.parsing.util.recognizing :as recognize]
            [pointer.core :as pointer]
            [such.maps :as map]))


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
   {:position (pointer/line-number-known ...),
    :expected-result-form '(just a 2),
    :expected-result (just a 2),
    :check-expectation :expect-match,
    :function-under-test (clojure.core/fn [] (cons a [2])),
    :description (midje.data.nested-facts/descriptions)}
   {:arrow '=>, :call-form '(cons a [2])}
   (hash-map :position (pointer/line-number-known 2)))
) ; ---------------------------------------------------------------

;;;                                             Checkable maps

(defn checkable-map? [value]
  (and (map? value)
       (::a-midje-checkable-map? value)))

(defn checkable
  [call-form arrow expected-result overrides]
  (let [source-details `{:call-form '~call-form
                         :arrow '~arrow }
        override-map `(hash-map ~@overrides)
        line (:line (meta call-form))
        result `(merge
                 {::a-midje-checkable-map? true
                  :function-under-test (fn [] ~call-form)
                  :expected-result ~expected-result
                  :check-expectation ~(recognize/expect-match-or-mismatch arrow)
                  :expected-result-form '~expected-result ;; This is also part of the source details.
                  :position (pointer/line-number-known ~line)
                  :namespace *ns*

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

(defn- choose-mkfn-for-arglist-matcher [arg-descriptions]
  (letfn [(allows-optional-args? [args] (shorthand/any? #(= % (symbol "&")) args))]
    (if (allows-optional-args? arg-descriptions)
      `(from-fake-maps/mkfn:arglist-matcher-allowing-optional-args ~@arg-descriptions)
      `(from-fake-maps/mkfn:arglist-matcher-fixed-arity ~@arg-descriptions))))

(defn fake [call-form fnref args arrow result overrides]
  (let [source-details `{:call-form '~call-form
                         :arrow '~arrow
                         :rhs '~(cons result overrides)}
        override-map `(hash-map ~@overrides)
        line (:line (meta call-form))
        result `(merge {:type :fake
                        :var ~(fnref/as-var-form fnref)
                        :value-at-time-of-faking (if (bound? ~(fnref/as-var-form fnref))
                                                   ~(fnref/as-form-to-fetch-var-value fnref))
                        :arglist-matcher ~(choose-mkfn-for-arglist-matcher args)
                        :result-supplier (from-fake-maps/mkfn:result-supplier ~arrow (fn [] ~result))
                        :times :default  ; Default allows for a more attractive error in the most common case.

                        :position (pointer/line-number-known ~line)
                        :namespace *ns*
                        :call-count-atom (atom 0)
                        :call-text-for-failures (str '~call-form)}
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
        override-map `(hash-map ~@overrides)
        line (:line (meta contained))
        result `(merge {:type :fake
                        :data-fake true
                        :var ~(fnref/as-var-form metaconstant)
                        :contained ~contained

                        :position (pointer/line-number-known ~line)
                        ;; kludje:
                        :call-count-atom (atom 1)}
                 ~source-details
                 ~override-map)]
    ;; pprint result
    result))

