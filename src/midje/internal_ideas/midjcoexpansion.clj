(ns midje.internal-ideas.midjcoexpansion
  (:use [midje.util.debugging :only [nopret]]
        [midje.util.wrapping :only [already-wrapped?]]
        [midje.util.form-utils :only [form-first?
				      preserve-type
				      ]]
        [midje.expect :only [expect?]]
        [midje.util.laziness :only [eagerly]]
        [midje.util.wrapping :only [
                                    wrappers
                                    for-wrapping-target?
                                    with-additional-wrappers
                                    multiwrap
                                    ]]
        [clojure.contrib.seq :only [separate]]
        [midje.fact :only [fact? future-fact?]]
        [midje.background :only [
                                 background-wrappers
                                 raw-wrappers
                                 background-form?]])
  )

(defn interior-forms [form]
  `(do ~@(rest (rest form))))


(defn forms-to-wrap-around [wrapping-target]
  (filter (for-wrapping-target? wrapping-target) (wrappers)))

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
