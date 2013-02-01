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

(defn- ^{:testable true } a-fake? [x]
  (and (seq? x)
       (semi-sweet-keyword? (first x))))

(defn mkfn:arrow? [& expected]
  (fn [actual] ((set expected) (name actual))))
(def normal-arrows? (mkfn:arrow? => =not=> =deny=>))
(def macroexpansion-arrow? (mkfn:arrow? =expands-to=>))
(def future-arrow? (mkfn:arrow? =future=>))

(defn expansion [call-form arrow expected-result fakes overrides]
  (pred-cond arrow
    normal-arrows?
    (let [check (lexical-maps/example call-form arrow expected-result overrides)]
      `(midje.checking.examples/check-one ~check ~(vec fakes)))
             
    macroexpansion-arrow?
    (let [expanded-macro `(macroexpand-1 '~call-form)
          escaped-expected-result `(quote ~expected-result)]
      (expansion expanded-macro => escaped-expected-result fakes
                 (concat overrides [:expected-result-form escaped-expected-result])))

    future-arrow?
    (let [position (:position (apply hash-map-duplicates-ok overrides))]
        `(emit/future-fact (nested-facts/descriptions ~(str "on `" call-form "`")) ~position))
    
    :else
    (throw (Error. (str "Program error: Unknown arrow form " arrow)))))

(defn to-lexical-map-form [full-form]
  (domonad validate-m [[call-form arrow expected-result & fakes+overrides] (validate full-form)
                       [fakes overrides] (separate a-fake? fakes+overrides)
                       _ (validate fakes)]
           (expansion call-form arrow expected-result fakes overrides)))

