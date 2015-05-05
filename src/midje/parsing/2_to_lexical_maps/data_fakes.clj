(ns ^{:doc "=contains=> prereqisites"}
  midje.parsing.2-to-lexical-maps.data-fakes
  (:use commons.clojure.core
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
  [metaconstant arrow contained overrides])

(def assert-valid! valid-pieces)

(defn to-lexical-map-form [a-list]
  (apply lexical-maps/data-fake (valid-pieces a-list)))

(defmacro data-fake
  "Creates a fake map that's used to associate key/value pairs with a metaconstant"
  {:arglists '([metaconstant arrow contained & overrides])}
  [& _]
  (to-lexical-map-form &form))


