(ns ^{:doc "Macros that provide less syntactic sugaring than those 
            from midje.sweet. midje.sweet is built on top of it."}
  midje.semi-sweet
  (:use midje.clojure.core
        midje.data.prerequisite-state
        [midje.util debugging]
        [midje.emission.deprecation :only [deprecate]]
        [midje.util.exceptions :only [user-error]]
        [midje.util.ecosystem :only [line-separator]]
        [midje.parsing.util.file-position :only [user-file-position]]
        midje.production-mode
        [clojure.algo.monads :only [domonad]]
        [clojure.string :only [join]])
  (:require [midje.util.pile :as pile]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.emission.api :as emit]
            midje.checking.examples
            [midje.parsing.util.fnref :as fnref]
            [midje.parsing.2-to-lexical-maps.examples :as parse-examples]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.parsing.2-to-lexical-maps.data-fakes :as parse-data-fakes]))
  

(immigrate 'midje.parsing.arrow-symbols)


;;; Interface: unfinished

(letfn [(unfinished* [names]
          (pile/macro-for [name names]
            `(do
               (defn ~name [& args#]
                 (let [pprint# (partial cl-format nil "~S")]
                   (throw (user-error (format "#'%s has no implementation, but it was called like this:%s(%s %s)" 
                                        '~name line-separator '~name (join " " (map pprint# args#)))))))
             
               ;; A reliable way of determining if an `unfinished` function has since been defined.
               (alter-meta! (var ~name) assoc :midje/unfinished-fun ~name)
               :ok)))]

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
  [& _]
  (deprecate "`fake` (and the entire midje.semi-sweet namespace) will be removed in 1.6.")
  (parse-fakes/to-lexical-map-form &form))

(defmacro data-fake
  "Creates a fake map that's used to associate key/value pairs with a metaconstant"
  {:arglists '([metaconstant arrow contained & overrides])}
  [& _]
  (deprecate "`data-fake` (and the entire midje.semi-sweet namespace) will be removed in 1.6.")
  (parse-data-fakes/to-lexical-map-form &form))

(defmacro expect 
  "Run the call form, check that all the mocks defined in the fakes 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument.

   To strip tests from production code, set either clojure.test/*load-tests*, 
   midje.semi-sweet/*include-midje-checks*, or midje.sweet/*include-midje-checks* to false."
  {:arglists '([call-form arrow expected-result & fakes+overrides])}
  [& _]
  (deprecate "`expect` (and the entire midje.semi-sweet namespace) will be removed in 1.6.")
  (when (user-desires-checking?)
    (parse-examples/to-lexical-map-form &form)))


(defmacro not-called
  "Creates an fake map that a function will not be called.
   Example: (not-called f))
   DEPRECATED: Prefer `:times 0` annotation to `fake`, ex. (provided (f) => irrelevant :times 0))"
  {:deprecated "1.3-alpha2"
   :arglists '([var-sym & overrides])}
  [var-sym & overrides]
  (deprecate "`not-called` (and the entire midje.semi-sweet namespace) will be removed in 1.6. Use `(provided (f) => irrelevant :times 0)` instead.")
  (let [fake-form `(fake (~var-sym) => "doesn't matter" ~@(concat overrides [:times 0]))]
    (with-meta fake-form {:line (meta &form)})))

