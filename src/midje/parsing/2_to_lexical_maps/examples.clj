(ns ^{:doc "generate a map for a particular example"}
  midje.parsing.2-to-lexical-maps.examples
  (:use [utilize.seq :only (separate find-first)]
        [midje.util.object-utils :only [object-name]]
        [midje.checkers :only [exactly]]
        [midje.checking.checkers.defining :only [checker? checker-makers]]
        [midje.parsing.1-to-normal-form.expects :only [expect? up-to-full-expect-form]]
        [midje.util.form-utils :only [first-named? translate-zipper map-difference
                                      hash-map-duplicates-ok pred-cond
                                      quoted-list-form? extended-fn?
                                      def-many-methods]]
        [midje.checking.extended-equality :only [extended-= extended-list-=]]
        [midje.parsing.util.file-position :only [user-file-position]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out
                                                   with-pushed-namespace-values
                                                   with-altered-roots]]
        [midje.parsing.util.wrapping :only [with-wrapping-target]]
        [midje.util.deprecation :only [deprecate]]
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

