(ns ^{:doc "Utility code for testing private vars."}
  midje.util
  (:use [midje.util.form-utils :only [macro-for]]))

(defmacro expose-testables
  "Enables testing of vars in the target ns which have ^:testable metadata"
  [target-ns]
  (macro-for [testable-sym (for [[sym var] (ns-interns target-ns)
                                 :when (:testable (meta var))]
                             sym) ]
    `(def ~testable-sym (intern '~target-ns '~testable-sym))))

(defmacro testable-privates 
  "Intern into the current namespace the symbols from the specified namespace"
  [namespace & symbols]
  (macro-for [sym symbols]
    `(def ~sym (intern '~namespace '~sym))))
