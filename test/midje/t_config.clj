(ns midje.t-config
  (:use [midje sweet util test-util])
  (:require [midje.config :as config]))


(fact "error-handling"
  (fact "can validate keys"
    (config/validate! {:unknown-key "value"}) 
    => (throws #"not configuration keys.*:unknown-key"))

  (fact "can use individual validation functions"
    (config/validate! {:print-level :unknown})
    => (throws #":unknown.*not a valid :print-level"))

  (fact "the appropriate functions call validate"
    (let [valid-map {:print-level :print-normally}]
      (config/with-temporary-config valid-map) => irrelevant
      (provided (config/validate! valid-map) => anything)

      (config/merge! valid-map) => irrelevant
      (provided (config/validate! valid-map) => anything))))


(fact "with-temporary-config"
  (config/with-temporary-config {:print-level :print-no-summary}
    (config/choice :print-level) => :print-no-summary
    (config/with-temporary-config {:print-level 0}
      (config/choice :print-level) => 0)))
  
(fact "assoc!"
  (config/assoc! :print-level :print-nothing)
  (config/choice :print-level) => :print-nothing)

(fact "merge!"
  (config/merge! {:print-level :print-nothing})
  (config/choice :print-level) => :print-nothing)
