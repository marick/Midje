(ns ^{:doc "generate a map for a particular example"}
  midje.parsing.2-to-lexical-maps.examples
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        [midje.util.object-utils :only [object-name]]
        [midje.checkers :only [exactly]]
        [midje.checking.checkers.defining :only [checker? checker-makers]]
        [midje.parsing.1-to-explicit-form.expects :only [expect? up-to-full-expect-form]]
        [midje.util.form-utils :only [map-difference
                                      pred-cond
                                      def-many-methods ]]
        [midje.checking.extended-equality :only [extended-= extended-list-=]]
        [midje.parsing.util.file-position :only [user-file-position]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out
                                                   with-altered-roots]]
        [midje.parsing.util.wrapping :only [with-wrapping-target]]
        [clojure.algo.monads :only [defmonad domonad]]
        [midje.parsing.arrow-symbols]
        midje.error-handling.validation-errors
        midje.error-handling.semi-sweet-validations
        [clojure.tools.macro :only [macrolet]])
  (:require [midje.data.metaconstant :as metaconstant]
            [midje.data.nested-facts :as nested-facts]
            [clojure.zip :as zip]
            [midje.config :as config]
            [midje.parsing.util.fnref :as fnref]
            [midje.error-handling.exceptions :as exceptions]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.emission.api :as emit])
  (:import midje.data.metaconstant.Metaconstant))

(defmulti expect-expansion (fn [_call-form_ arrow & _rhs_]
                             (name arrow)))

(def-many-methods expect-expansion [=> =not=> =deny=>]
  [call-form arrow expected-result fakes overrides]
  `(let [check# (lexical-maps/example ~call-form ~arrow ~expected-result ~overrides)]
     (midje.checking.examples/check-one check# ~fakes)))

(defmethod expect-expansion =expands-to=>
  [call-form _arrow_ expected-result fakes overrides]
  (let [expanded-macro `(macroexpand-1 '~call-form)
        escaped-expected-result `(quote ~expected-result)]
    `(let [check# (lexical-maps/example ~expanded-macro => ~escaped-expected-result
                                     ~(concat overrides [:expected-result-form escaped-expected-result]))]
       (midje.checking.examples/check-one check# ~fakes))))

(defmethod expect-expansion =future=>
  [call-form arrow expected-result _fakes_ overrides]
  `(let [check# (lexical-maps/example ~call-form ~arrow ~expected-result ~overrides)]
     (emit/future-fact (nested-facts/descriptions ~(str "on `" call-form "`"))
                       (:position check#))))

(defn- ^{:testable true } a-fake? [x]
  (and (seq? x)
       (semi-sweet-keyword? (first x))))

(defn to-lexical-map-form [full-form]
  (domonad validate-m [[call-form arrow expected-result & fakes+overrides] (validate full-form)
                       [fakes overrides] (separate a-fake? fakes+overrides)
                       _ (validate fakes)]
           (expect-expansion call-form arrow expected-result (vec fakes) overrides)))
