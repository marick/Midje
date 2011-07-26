(ns midje.ideas.facts
  (:use [midje.util.form-utils :only [form-first? translate preserve-type]]
        [midje.unprocessed :only [with-installed-fakes]]
        [midje.internal-ideas.fakes :only [unfold-fakes]]

        [midje.util.laziness :only [eagerly]]
        [midje.internal-ideas.expect :only [tack-on__then__at-rightmost-expect-leaf
                                            wrap-with-expect__then__at-rightmost-expect-leaf
                                            expect?]]
        [midje.internal-ideas.file-position :only [set-fallback-line-number-from
                                                   annotate-embedded-arrows-with-line-numbers]]
        [midje.internal-ideas.wrapping :only [already-wrapped?
                                              multiwrap
                                              wrappers
                                              with-additional-wrappers
                                              for-wrapping-target?
                                              put-wrappers-into-effect
                                              forms-to-wrap-around]]
        [midje.util.debugging :only [nopret]]
        [midje.semi-sweet :only [is-semi-sweet-keyword?]]
        [midje.ideas.prerequisites :only [is-head-of-form-providing-prerequisites?
                                    expand-prerequisites-into-fake-calls
                                    delete_prerequisite_form__then__at-previous-full-expect-form]]
        [midje.ideas.arrows :only [is-start-of-arrow-sequence?]]
        [clojure.contrib.seq :only [separate]]
        [midje.ideas.background :only [background-fakes
                                       raw-wrappers
                                       background-wrappers
                                       background-form?]]
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


(defn to-semi-sweet [multi-form]
  (translate multi-form
    is-start-of-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf
    
    is-head-of-form-providing-prerequisites?
    (fn [loc ] (let [fake-calls (expand-prerequisites-into-fake-calls loc)
                    full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
                 (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form)))

    is-semi-sweet-keyword?
    skip-to-rightmost-leaf))


(defn interior-forms [form]
  `(do ~@(rest (rest form))))


(defn midjcoexpand [form]
  ;; (p+ "== midjcoexpanding" form)
  ;; (p "== with" (wrappers))
  (nopret (cond (already-wrapped? form)
        form

        (form-first? form "quote")
        form

        (future-fact? form)
        (macroexpand form)

        (expect? form)
        (multiwrap form (forms-to-wrap-around :checks))

        (fact? form)
        (do
          (multiwrap (midjcoexpand (macroexpand form))
                     (forms-to-wrap-around :facts)))

        (background-form? form)
        (do
          ;; (p+ "use these wrappers" (raw-wrappers form))
          ;; (p "for this form" (interior-forms form))
          ;; (p (wrappers))
          (nopret (let [wrappers (background-wrappers (raw-wrappers form))
                      [now-wrappers later-wrappers] (separate (for-wrapping-target? :contents)
                                                              wrappers)]
            ;; "Now wrappers" have to be separated out and discarded here, because
            ;; if they were left in, they'd be reapplied in any nested background
            ;; forms.
            ;; (p "now-wrappers" now-wrappers)
            ;; (p "later-wrappers" later-wrappers)
            (multiwrap (with-additional-wrappers later-wrappers
                          (midjcoexpand (interior-forms form)))
                       now-wrappers))))
        
        (sequential? form)
        (preserve-type form (eagerly (map midjcoexpand form)))

        :else
        form)))


(defn- surround-with-background-fakes [forms]
  `(with-installed-fakes (background-fakes)
     (do ~@forms)))

(defn expand-and-transform [forms]
  (let [form-to-run (-> forms
                        annotate-embedded-arrows-with-line-numbers
                        to-semi-sweet
                        unfold-fakes
                        surround-with-background-fakes
                        midjcoexpand
                        (multiwrap (forms-to-wrap-around :facts)))]
    (define-metaconstants form-to-run)
    (report/form-providing-friendly-return-value form-to-run)))
