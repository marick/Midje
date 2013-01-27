(ns ^{:doc "generate a map for a particular example"}
  midje.parsing.2-to-lexical-maps.examples
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        [clojure.algo.monads :only [domonad]]
        [midje.parsing.arrow-symbols]
        midje.error-handling.validation-errors
        midje.error-handling.semi-sweet-validations)
  (:require [clojure.zip :as zip]
            [midje.config :as config]
            [midje.util.pile :as pile]
            [midje.error-handling.exceptions :as exceptions]
            [midje.data.metaconstant :as metaconstant]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.emission.api :as emit])
  (:import midje.data.metaconstant.Metaconstant))

(defmulti expect-expansion (fn [_call-form_ arrow & _rhs_]
                             (name arrow)))

(pile/def-many-methods expect-expansion [=> =not=> =deny=>]
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
