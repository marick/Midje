(ns ^{:doc "Facts are the core abstraction of Midje."}
  midje.ideas.facts
  (:use [midje.error-handling.validation-errors :only [simple-validation-error-report-form validate when-valid]]
        [midje.util.namespace :only [semi-sweet-keyword?]]
        [midje.internal-ideas.fakes :only [unfold-fakes]]

        [midje.internal-ideas.expect :only [expect?
                                            wrap-with-expect__then__at-rightmost-expect-leaf]]
        [midje.internal-ideas.file-position :only [annotate-embedded-arrows-with-line-numbers]]
        [midje.internal-ideas.fact-context :only [within-runtime-fact-context]]
        [midje.internal-ideas.wrapping :only [already-wrapped?
                                              multiwrap
                                              with-additional-wrappers
                                              forms-to-wrap-around]]
        [midje.util.debugging :only [nopret]]
        [midje.ideas.prerequisites :only [head-of-form-providing-prerequisites?
                                          insert-prerequisites-into-expect-form-as-fakes]]
        [midje.ideas.arrows :only [start-of-checking-arrow-sequence? leaves-contain-arrow?]]
        [midje.ideas.background :only [surround-with-background-fakes
                                       body-of-against-background
                                       against-background-contents-wrappers
                                       against-background-facts-and-checks-wrappers
                                       against-background?]]
        [midje.ideas.metaconstants :only [define-metaconstants]] 
        [midje.util.form-utils :only [def-many-methods first-named? translate-zipper
                                      preserve-type quoted? pred-cond reader-line-number named?]]
        [midje.util.laziness :only [eagerly]]
        [midje.util.zip :only [skip-to-rightmost-leaf]]
        [swiss-arrows.core :only [-<>]])
  (:require [clojure.zip :as zip])
  (:require [midje.ideas.reporting.report :as report]))
(declare midjcoexpand separate-fact-metadata)

(defn fact? [form]
  (or (first-named? form "fact")
      (first-named? form "facts")))

(def future-prefixes ["future-" 
                      "pending-" 
                      "incipient-" 
                      "antiterminologicaldisintactitudinarian-"])

(def future-fact-variant-names (for [prefix future-prefixes
                                     fact-or-facts ["fact" "facts"]]
                                 (str prefix fact-or-facts)))

(defn future-fact? [form]
  (some (partial first-named? form) future-fact-variant-names ))

(defn future-fact* [form]
  (let [lineno (reader-line-number form)
        [metadata _] (separate-fact-metadata form)]
    `(within-runtime-fact-context ~(:midje/description metadata)
       (clojure.test/report {:type :future-fact
                             :description @midje.internal-ideas.fact-context/nested-descriptions
                             :position (midje.internal-ideas.file-position/line-number-known ~lineno)}))))

(defn to-semi-sweet
  "Convert sweet keywords into their semi-sweet equivalents.
   1) Arrow sequences become expect forms.
   2) (provided ...) become fakes inserted into preceding expect."
  [multi-form]
  (translate-zipper multi-form
    start-of-checking-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf
    
    head-of-form-providing-prerequisites?
    insert-prerequisites-into-expect-form-as-fakes

    semi-sweet-keyword?
    skip-to-rightmost-leaf))

(defn midjcoexpand
  "Descend form, macroexpanding *only* midje forms and placing background wrappers where appropriate."
  [form]
  (pred-cond form
    already-wrapped?     form
    quoted?              form
    future-fact?         (macroexpand form)
    against-background?  (when-valid form
                             (-<> form 
                                  body-of-against-background
                                  midjcoexpand
                                  (with-additional-wrappers (against-background-facts-and-checks-wrappers form) <>)
                                  (multiwrap <> (against-background-contents-wrappers form))))
  
    expect?      (multiwrap form (forms-to-wrap-around :checks ))
    fact?        (macroexpand form)
    sequential?  (preserve-type form (eagerly (map midjcoexpand form)))
    :else        form))

;; The rather hackish construction here is to keep
;; the expanded fact body out of square brackets because
;; `tabular` expansions use `seq-zip`. 
(defn define-and-run [metadata expanded-form]
  `((fn [function#] (function#))
    (with-meta (fn [] ~expanded-form) '~metadata)))

(defn complete-fact-transformation [metadata forms]
  (let [form-to-run (-> forms
                        annotate-embedded-arrows-with-line-numbers
                        to-semi-sweet
                        unfold-fakes
                        surround-with-background-fakes
                        midjcoexpand
                        (multiwrap (forms-to-wrap-around :facts)))]
    (define-metaconstants form-to-run)
    (define-and-run metadata
      (report/form-providing-friendly-return-value 
       `(within-runtime-fact-context ~(:midje/description metadata) ~form-to-run)))))



(def-many-methods validate ["fact" "facts"] [[fact-or-facts & args :as form]]
  ;; Removed the check for no arrow because (1) it gives me too many false
  ;; positives and (2) doesn't fit with new handling of tabulate. Replace
  ;; someday with a version that correctly detects forms like this:
  ;; (fact (cons 1 => 2))
  ;; ... which are the ones I most often mess up.
  ;; (if-not (leaves-contain-arrow? (rest form))
  ;;   (simple-validation-error-report-form form
  ;;     (format "There is no arrow in your %s form:" (name fact-or-facts)))))
  )

            
(defn separate-fact-metadata [fact-form]
  (letfn [(basic-parse [metadata body]
            (let [head (first body)
                  add-key (fn [key value] (assoc metadata key value))]

              (cond (string? head)
                    (recur (add-key :midje/description head) (rest body))

                    (start-of-checking-arrow-sequence? body)
                    [metadata body]

                    (symbol? head)
                    (recur (add-key :midje/name (name head)) (rest body))

                    (keyword? head)
                    (recur (add-key head true) (rest body))

                    (map? head)
                    (recur (merge metadata head) (rest body))
                    
                    :else
                    [metadata body])))]
    (let [[metadata body] (basic-parse {:midje/source fact-form}
                                       (rest fact-form))
          metadata (if (and (contains? metadata :midje/description)
                            (not (contains? metadata :midje/name)))
                     (assoc metadata :midje/name (:midje/description metadata))
                     metadata)]
      [metadata body])))

