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
                            (cl-format nil "In `~A ~A ~A`, ~A is not a metaconstant."
                                       metaconstant arrow contained metaconstant)))

        ;; This one isn't possible because the right-hand-side could be a bound symbol.
        ;; (not (map? contained))
        ;; (error/report-error form 
        ;;                     "The right-hand side of a =contains=> arrow should be a map."))
  
  [metaconstant arrow contained overrides])

(def assert-valid! valid-pieces)

(defn to-lexical-map-form [a-list]
  (apply lexical-maps/data-fake (valid-pieces a-list)))
