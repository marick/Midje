;;; This namespace is mainly responsible for converting particular macros
;;; into the arguments used by midje.unprocessed's `expect*`.

(ns ^{:doc "Macros that provide less syntactic sugaring than those 
            from midje.sweet. midje.sweet is built on top of it."}
  midje.semi-sweet
  (:use clojure.test
        midje.internal-ideas.fakes
        midje.internal-ideas.file-position
        [midje.util debugging form-utils namespace]
        [midje.util.deprecation :only [deprecate]]
        midje.error-handling.validation-errors
        midje.error-handling.semi-sweet-validations
        [midje.error-handling.exceptions :only [user-error]]
        [midje.util.namespace :only [semi-sweet-keyword?]]
        [midje.util.ecosystem :only [line-separator]]
        midje.production-mode
        [clojure.algo.monads :only [domonad]]
        clojure.pprint
        [clojure.string :only [join]])
  (:require [midje.internal-ideas.fact-context :as fact-context]))

(immigrate 'midje.unprocessed)
(immigrate 'midje.ideas.arrow-symbols)

;;; Conversions to unprocessed form

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn- ^{:testable true } check-for-arrow [arrow]
  (condp = (name arrow) 
    => :check-match
    =expands-to=> :check-match
    =not=> :check-negated-match
    =deny=> :check-negated-match
    nil))

(defmacro unprocessed-check
  "Creates a map that contains a function-ized version of the form being 
   tested, an expected result, and the file position to refer to in case of 
   failure. See 'expect*'."
  [call-form arrow expected-result overrides]
  `(merge
    {:description (fact-context/nested-descriptions)
     :function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :desired-check ~(check-for-arrow arrow)
     :expected-result-text-for-failures '~expected-result
     :position (user-file-position)
     
     ;; for Midje tool creators:
     :call-form '~call-form
     :arrow '~arrow }
     (hash-map-duplicates-ok ~@overrides)))

(defmulti ^{:private true} expect-expansion (fn [_call-form_ arrow & _rhs_]
                                              (name arrow)))

(def-many-methods expect-expansion [=> =not=> =deny=>]
  [call-form arrow expected-result fakes overrides]
  `(let [check# (unprocessed-check ~call-form ~arrow ~expected-result ~overrides)]
     (midje.semi-sweet/*expect-checking-fn* check# ~fakes)))

(defmethod expect-expansion =expands-to=>
  [call-form _arrow_ expected-result fakes overrides]
  (let [expanded-macro `(macroexpand-1 '~call-form)
        escaped-expected-result `(quote ~expected-result)]
    (expect-expansion expanded-macro => escaped-expected-result fakes overrides)))

(defmethod expect-expansion =future=>
  [call-form arrow expected-result _fakes_ overrides]
  `(let [check# (unprocessed-check ~call-form ~arrow ~expected-result ~overrides)]
     (clojure.test/report {:type :future-fact
                           :description (fact-context/nested-descriptions
                                         ~(str "on `" call-form "`"))
                           :position (:position check#)})))

;;; Interface: unfinished

(letfn [(unfinished* [names]
          (macro-for [name names]
            `(do
               (defn ~name [& args#]
                 (let [pprint# (partial cl-format nil "~S")]
                   (throw (user-error (format "#'%s has no implementation, but it was called like this:%s(%s %s)" 
                                        '~name line-separator '~name (join " " (map pprint# args#)))))))
             
               ;; A reliable way of determining if an `unfinished` function has since been defined.
               (alter-meta! (var ~name) assoc :midje/unfinished-fun ~name))))]

  (defmacro unfinished
    "Defines a list of names as functions that have no implementation yet. They will
     throw Errors if ever called."
    [& names] (unfinished* names))
  
  (defmacro only-mocked 
    "Defines a list of names as functions that have no implementation yet. They will
     throw Errors if ever called.
     DEPRECATED: Prefer `unfinished`."
    {:deprecated "1.3-alpha2"}
    [& names]
    (deprecate "`only-mocked` will be removed in version 1.6. Use `unfinished` instead.")
    (unfinished* names)))



;;; Interface: production mode

(defonce
  ^{:doc "True by default.  If set to false, Midje checks are not
     included into production code, whether compiled or loaded."
     :dynamic true}
  *include-midje-checks* true)


;;; Interface: Main macros

(defmacro fake 
  "Creates a fake map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  {:arglists '([call-form arrow result & overrides])}
  [& forms]
  (when-valid &form (fake* forms)))

(defmacro data-fake
  "Creates a fake map that's used to associate key/value pairs with a metaconstant"
  {:arglists '([metaconstant arrow contained & overrides])}
  [& forms]
  (when-valid &form (data-fake* forms)))

(defmacro not-called
  "Creates an fake map that a function will not be called.
   Example: (not-called f))
   DEPRECATED: Prefer `:times 0` annotation to `fake`, ex. (provided (f) => 4 :times 0))"
  {:deprecated "1.3-alpha2"
   :arglists '([var-sym & overrides])}
  [forms]
  (deprecate "`not-called` will be removed in 1.6. Use `(provided (f) => 4 :times 0)` instead.")
  (not-called* forms))

(defn- ^{:testable true } a-fake? [x]
  (and (seq? x)
       (semi-sweet-keyword? (first x))))

(defmacro expect 
  "Run the call form, check that all the mocks defined in the fakes 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument.

   To strip tests from production code, set either clojure.test/*load-tests*, 
   midje.semi-sweet/*include-midje-checks*, or midje.sweet/*include-midje-checks* to false."
  {:arglists '([call-form arrow expected-result & fakes+overrides])}
  [& _]
  (when (user-desires-checking?)
    (domonad validate-m [[call-form arrow expected-result & fakes+overrides] (validate &form)
                         [fakes overrides] (separate-by a-fake? fakes+overrides)
                         _ (validate fakes)]
      (expect-expansion call-form arrow expected-result fakes overrides))))

(def ^{:dynamic true
       :doc (str "For Midje tool creators. Hooks into Midje's internal compiler results.
  Can be bound to a function with arglists like:" line-separator
              "  " (:arglists (meta #'midje.unprocessed/expect*)))}
  *expect-checking-fn* midje.unprocessed/expect*)
