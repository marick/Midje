(ns ^{:doc "Utility code for testing private vars."}
  midje.util
  (:require [midje.util.pile :as pile]))

(defmacro expose-testables
  "Enables testing of vars in the target ns which have ^:testable metadata.
   Note that the namespace names should not be quoted."
  [target-ns]
  (pile/macro-for [[sym var] (ns-interns target-ns)
                   :when (:testable (meta var))]
                  `(-> (def ~sym ~var)
                       (alter-meta! merge (meta ~var)))))

(defmacro testable-privates
  "Intern into the current namespace the symbols from the specified namespace"
  [namespace & symbols]
  (pile/macro-for [sym symbols, :let [var (ns-resolve namespace sym)]]
                  `(-> (def ~sym ~var)
                       (alter-meta! merge
                                    (assoc (meta ~var)
                                           :testable true)))))
