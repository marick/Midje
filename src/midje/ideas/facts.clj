(ns midje.ideas.facts
  (:use [midje.util.form-utils :only [form-first? translate preserve-type]]
        [midje.semi-sweet :only [is-semi-sweet-keyword?]]
        [midje.internal-ideas.fakes :only [unfold-fakes]]

        [midje.util.laziness :only [eagerly]]
        [midje.util.namespace :only [namespacey-match]]
        [midje.internal-ideas.expect :only [expect?
                                            wrap-with-expect__then__at-rightmost-expect-leaf]]
        [midje.internal-ideas.file-position :only [annotate-embedded-arrows-with-line-numbers]]
        [midje.internal-ideas.wrapping :only [already-wrapped?
                                              multiwrap
                                              with-additional-wrappers
                                              forms-to-wrap-around]]
        [midje.util.debugging :only [nopret]]
        [midje.ideas.prerequisites :only [is-head-of-form-providing-prerequisites?
                                          insert-prerequisites-into-expect-form-as-fakes]]
        [midje.ideas.arrows :only [is-start-of-arrow-sequence?]]
        [clojure.contrib.seq :only [separate]]
        [midje.ideas.background :only [surround-with-background-fakes
                                       against-background-body
                                       against-background-contents-wrappers
                                       against-background-children-wrappers
                                       against-background?]]
        [midje.ideas.metaconstants :only [define-metaconstants]]
        [midje.util.zip :only [skip-to-rightmost-leaf]])
  (:require [clojure.zip :as zip])
  (:require [midje.util.report :as report]))
(declare midjcoexpand)

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


(defn to-semi-sweet
  "Convert sweet keywords into their semi-sweet equivalents.
   1) Arrow sequences become expect forms.
   2) (provided ...) become fakes inserted into preceding expect."
  [multi-form]
  (translate multi-form
    is-start-of-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf
    
    is-head-of-form-providing-prerequisites?
    insert-prerequisites-into-expect-form-as-fakes

    is-semi-sweet-keyword?
    skip-to-rightmost-leaf))

(defn- expand-against-background [form wrappers]
  (with-additional-wrappers wrappers
    (midjcoexpand form)))

(defn midjcoexpand
  "Descend form, macroexpanding *only* midje forms and placing background wrappers where appropriate."
  [form]
  (cond (already-wrapped? form)
        form

        (form-first? form "quote")
        form

        (future-fact? form)
        (macroexpand form)

        (against-background? form)
        (-> (expand-against-background (against-background-body form)
                                       (against-background-children-wrappers form))
            (multiwrap (against-background-contents-wrappers form)))
        
        (expect? form)
        (multiwrap form
                   (forms-to-wrap-around :checks))

        (fact? form)
        (multiwrap (midjcoexpand (macroexpand form))
                   (forms-to-wrap-around :facts))

        (sequential? form)
        (preserve-type form (eagerly (map midjcoexpand form)))

        :else
        form))


(defn complete-fact-transformation [forms]
  (let [form-to-run (-> forms
                        annotate-embedded-arrows-with-line-numbers
                        to-semi-sweet
                        unfold-fakes
                        surround-with-background-fakes
                        midjcoexpand
                        (multiwrap (forms-to-wrap-around :facts)))]
    (define-metaconstants form-to-run)
    (report/form-providing-friendly-return-value form-to-run)))
