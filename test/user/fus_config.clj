(ns user.fus-config
  (:require [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.config :as config]))

(fact "change-defaults operates on the root binding"
  (let [stashed-config config/*config*
        original-print-level (config/choice :print-level)]
    (try
      original-print-level =not=> :print-nothing
      (config/with-augmented-config {:print-level :print-facts}
        ;; emphasizes that changes override the root binding
        (config/change-defaults :print-level :print-nothing))
      (config/choice :print-level) => original-print-level
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
        (config/with-augmented-config valid-map) => irrelevant
        (provided (config/validate! valid-map) => anything)

        (config/merge-permanently! valid-map) => irrelevant
        (provided (config/validate! valid-map) => anything)

        (config/change-defaults :print-level :print-normally) => irrelevant
        (provided (config/validate! valid-map) => anything)

      (finally
       (config/merge-permanently! stashed-config))))))



(fact "with-augmented-config"
  (config/with-augmented-config {:print-level :print-no-summary}
    (config/choice :print-level) => :print-no-summary
    (config/with-augmented-config {:print-level 0}
      (config/choice :print-level) => 0)))



(facts "about converting filters into functions"
  (let [a-fact (fn [metadata] (with-meta '[] metadata))]
    (fact "keywords check for the truthiness of the key in the metadata"
      (let [fun (config/mkfn:fact-filter-predicate [:property])]
        (fun (a-fact {:property 'truthy})) => truthy
        (fun (a-fact {:property false})) => falsey
        (fun (a-fact {})) => falsey))

    (fact "regexes check the fact's name property"
      (let [fun (config/mkfn:fact-filter-predicate [#"regex"])]
        (fun (a-fact {:midje/name "something containing regex."})) => truthy
        (fun (a-fact {:midje/name "not a match"})) => falsey
        (fun (a-fact {})) => falsey))

    (fact "strings are treated as substrings"
      (let [fun (config/mkfn:fact-filter-predicate ["str"])]
        (fun (a-fact {:midje/name "something str like"})) => truthy
        (fun (a-fact {:midje/name "not a match"})) => falsey
        (fun (a-fact {})) => falsey))

    (fact "functions are applied to arguments"
      (let [fun (config/mkfn:fact-filter-predicate [(fn [meta] (= "yes" (:something meta)))])]
        (fun (a-fact {:something "yes"})) => truthy
        (fun (a-fact {:something "no"})) => falsey
        (fun (a-fact {})) => falsey))

    (fact "multiple arguments are OR'd together"
      (let [fun (config/mkfn:fact-filter-predicate [#"foo" :valiant])]
        (fun (a-fact {:midje/name "ofoop"})) => truthy
        (fun (a-fact {:valiant true})) => truthy
        (fun (a-fact {})) => falsey))

    (fact "filter predicates know why they were created"
      (meta (config/mkfn:fact-filter-predicate [:oddity :valiant]))
      => (contains {:created-from [:oddity :valiant]}))

    (fact "If there are no filter arguments, the fact filter is constructed from the default"
      ;; Note that the default is not aware it works on metadata.
      (config/with-augmented-config {:fact-filter :has-this-meta-key}
        (let [fun (config/mkfn:fact-filter-predicate [])]
          (fun (a-fact {})) => falsey
          (fun (a-fact {:has-this-meta-key true})) => truthy
          (meta fun) => (contains {:created-from [:has-this-meta-key]}))))))
