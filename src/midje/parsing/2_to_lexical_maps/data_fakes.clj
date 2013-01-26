(ns ^{:doc "=contains=> prereqisites"}
  midje.parsing.2-to-lexical-maps.data-fakes
  (:use midje.clojure.core
        midje.error-handling.validation-errors
        midje.error-handling.semi-sweet-validations)
  (:require [midje.parsing.lexical-maps :as lexical-maps]))


(defn to-lexical-map-form [a-list]
  (when-valid a-list
    (let [[_ metaconstant arrow contained & overrides] a-list]
      (lexical-maps/data-fake metaconstant arrow contained overrides))))

