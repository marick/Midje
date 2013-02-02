(ns ^{:doc "=contains=> prereqisites"}
  midje.parsing.2-to-lexical-maps.data-fakes
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.arrow-symbols)
  (:require [midje.parsing.lexical-maps :as lexical-maps]
            [midje.parsing.util.error-handling :as error]
            [midje.data.metaconstant :as metaconstant]))

(defn valid-pieces [[_data-fake_ metaconstant arrow contained & overrides :as form]]
  (cond (not (metaconstant/metaconstant-symbol? metaconstant))
        (error/report-error form
          "You seem to be assigning values to a metaconstant, but there's no metaconstant."))

        ;; This one isn't possible because the right-hand-side could be a bound symbol.
        ;; (not (map? contained))
        ;; (error/report-error form 
        ;;                     "The right-hand side of a =contains=> arrow should be a map."))
  
  [metaconstant arrow contained overrides])

(defn to-lexical-map-form [a-list]
  (apply lexical-maps/data-fake (valid-pieces a-list)))
