(ns ^{:doc "Macros that provide less syntactic sugaring than those 
            from midje.sweet. midje.sweet is built on top of it."}
  midje.semi-sweet
  (:use midje.internal-ideas.fakes
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
  (:require [midje.internal-ideas.fact-context :as fact-context]
            [midje.parsing.map-templates :as map-templates]
            [midje.emission.api :as emit]
            [midje.parsing.fakes :as parse-fakes]))

(immigrate 'midje.checking.examples)
(immigrate 'midje.ideas.arrow-symbols)

;;; Conversions to unprocessed form


(defmulti ^{:private true} expect-expansion (fn [_call-form_ arrow & _rhs_]
                                              (name arrow)))

(def-many-methods expect-expansion [=> =not=> =deny=>]
  [call-form arrow expected-result fakes overrides]
  `(let [check# (map-templates/example ~call-form ~arrow ~expected-result ~overrides)]
     (midje.semi-sweet/*expect-checking-fn* check# ~fakes)))

(defmethod expect-expansion =expands-to=>
  [call-form _arrow_ expected-result fakes overrides]
  (let [expanded-macro `(macroexpand-1 '~call-form)
        escaped-expected-result `(quote ~expected-result)]
    `(let [check# (map-templates/example ~expanded-macro => ~escaped-expected-result
                                     ~(concat overrides [:expected-result-form escaped-expected-result]))]
       (midje.semi-sweet/*expect-checking-fn* check# ~fakes))))

(defmethod expect-expansion =future=>
  [call-form arrow expected-result _fakes_ overrides]
  `(let [check# (map-templates/example ~call-form ~arrow ~expected-result ~overrides)]
     (emit/future-fact (fact-context/nested-descriptions ~(str "on `" call-form "`"))
                       (:position check#))))

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
       :doc (str "For Midje tool creators. Hooks into Midje's internal compiler results."
                 line-separator
                 "Can be bound to a function with arglists like:"
                 line-separator
                 "  "
                 (:arglists (meta #'midje.checking.examples/check-one)))}
  *expect-checking-fn* midje.checking.examples/check-one)
