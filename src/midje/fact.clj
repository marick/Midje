(ns midje.fact
  (:use [midje.util.form-utils :only [form-first? translate]]
        [midje.expect :only [tack-on__then__at-rightmost-expect-leaf
                             wrap-with-expect__then__at-rightmost-expect-leaf
                             ]]
        [midje.semi-sweet :only [is-semi-sweet-keyword?]]
        [midje.prerequisites :only [is-head-of-form-providing-prerequisites?
                                    expand-prerequisites-into-fake-calls
                                    delete_prerequisite_form__then__at-previous-full-expect-form]]
        [midje.arrows :only [is-start-of-arrow-sequence?]]
        [midje.util.zip :only [skip-to-rightmost-leaf]])
  (:require [clojure.zip :as zip]))

(defn fact? [form]
  (or (form-first? form "fact")
      (form-first? form "facts")))
(defn future-fact? [form]
  (or (form-first? form "future-fact")
      (form-first? form "future-facts")
      (form-first? form "pending-fact")
      (form-first? form "pending-facts")
      (form-first? form "incipient-fact")
      (form-first? form "incipient-facts")
      (form-first? form "antiterminologicaldisintactitudinarian-fact")
      (form-first? form "antiterminologicaldisintactitudinarian-facts")))


(defn translate-fact-body [multi-form]
  (translate multi-form
    is-start-of-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf
    
    is-head-of-form-providing-prerequisites?
    (fn [loc ] (let [fake-calls (expand-prerequisites-into-fake-calls loc)
                    full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
                 (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form)))

    is-semi-sweet-keyword?
    skip-to-rightmost-leaf))

