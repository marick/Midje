(ns midje.t-config
  (:use [midje sweet util test-util])
  (:require [midje.config :as config]))

(fact "overriding defaults"
  (let [stashed-config config/*config*]
    (try
      (config/choice :print-level) =not=> :print-nothing
      (config/with-temporary-config {:print-level :print-facts}
        ;; emphasizes that override overrides the root binding
        (config/override-with :print-level :print-nothing))
      (config/choice :print-level) => :print-nothing
    (finally
     (config/merge-permanently! stashed-config)))))

(fact "error-handling"
  (fact "can validate keys"
    (config/validate! {:unknown-key "value"}) 
    => (throws #"not configuration keys.*:unknown-key"))

  (fact "can use individual validation functions"
    (config/validate! {:print-level :unknown})
    => (throws #":unknown.*not a valid :print-level"))

  (fact "the appropriate functions call validate"
    (let [stashed-config config/*config*
          valid-map {:print-level :print-normally}]
      (try
        (config/with-temporary-config valid-map) => irrelevant
        (provided (config/validate! valid-map) => anything)

        (config/merge-permanently! valid-map) => irrelevant
        (provided (config/validate! valid-map) => anything)

        (config/override-with :print-level :print-normally) => irrelevant
        (provided (config/validate! valid-map) => anything)

      (finally
       (config/merge-permanently! stashed-config))))))



(fact "with-temporary-config"
  (config/with-temporary-config {:print-level :print-no-summary}
    (config/choice :print-level) => :print-no-summary
    (config/with-temporary-config {:print-level 0}
      (config/choice :print-level) => 0)))

