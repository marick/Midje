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
        [midje.ideas.metadata :only [separate-metadata]]
        [midje.util.form-utils :only [def-many-methods first-named? translate-zipper
                                      preserve-type quoted? pred-cond reader-line-number named?]]
        [midje.util.laziness :only [eagerly]]
        [midje.util.zip :only [skip-to-rightmost-leaf]]
        [swiss-arrows.core :only [-<>]])
  (:require [clojure.zip :as zip])
  (:require [midje.internal-ideas.compendium :as compendium]
            midje.ideas.reporting.report))

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
        [metadata _] (separate-metadata form)]
    `(within-runtime-fact-context ~(:midje/description metadata)
       (clojure.test/report {:type :future-fact
                             :description @midje.internal-ideas.fact-context/nested-descriptions
                             :position (midje.internal-ideas.file-position/line-number-known ~lineno)}))))

                                ;;; Fact processing

;; There are three stages to fact processing:
;; * Body processing: the convertion of arrow and provided forms into their
;;   semi-sweet forms, the insertion of background data, line numbering, etc.
;;   
;; * Compendium processing: wrapping the body in a function form that supports
;;   rerunning and history keeping.
;;   
;; * Load-time processing: wrapping the final form in code that does whatever
;;   should be done the first time the fact is loaded. (Such as running it for
;;   the first time.)

;;; Support code 

(def ^{:dynamic true} *parse-time-fact-level* 0)

(defn- working-on-top-level-fact? []
  (= *parse-time-fact-level* 1))
  
(defmacro given-possible-fact-nesting [& forms]
  `(binding [*parse-time-fact-level* (inc *parse-time-fact-level*)]
     ~@forms))

(defmacro working-on-nested-facts [& forms]
  ;; Make sure we don't treat this as a top-level fact
  `(binding [*parse-time-fact-level* (+ 2 *parse-time-fact-level*)]
     ~@forms))

;;; Body Processing

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

(defn expand-fact-body [forms metadata]
  (-> forms
      annotate-embedded-arrows-with-line-numbers
      to-semi-sweet
      unfold-fakes
      surround-with-background-fakes
      midjcoexpand
      (multiwrap (forms-to-wrap-around :facts))))


;;; Check-time processing

(defn wrap-with-check-time-code
  ([expanded-body metadata]
     (wrap-with-check-time-code expanded-body metadata
                                               (gensym 'this-function-here-)))

  ;; Having two versions lets tests not get stuck with a gensym.
  ([expanded-body metadata this-function-here-symbol]
     `(letfn [(base-function# [] ~expanded-body)
              (~this-function-here-symbol []
                (with-meta base-function#
                  (merge '~metadata
                         {:midje/top-level-fact? ~(working-on-top-level-fact?)})))]
        (~this-function-here-symbol))))

;;; Load-time processing

(declare check-one)

(defn wrap-with-creation-time-code [function-form]
  (letfn [;; The rather hackish construction here is to keep
          ;; the expanded fact body out of square brackets because
          ;; `tabular` expansions use `seq-zip`. 
          (wrap-with-creation-time-fact-recording [function-form]
            (if (working-on-top-level-fact?)
              `(creation-time-fact-processing ~function-form)
              function-form))
          
          (run-after-creation [function-form]
            `(check-one ~function-form))]

    (define-metaconstants function-form)
    (-> function-form
        wrap-with-creation-time-fact-recording
        run-after-creation)))

;;; There could be validation here, but none has proven useful.

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


;;; Ta-da!

(defn complete-fact-transformation [metadata forms]
  (given-possible-fact-nesting
   (-> forms
       (expand-fact-body metadata)
       (wrap-with-check-time-code metadata)
       wrap-with-creation-time-code)))


;;; Fact execution utilities


(defn creation-time-fact-processing [fact-function]
  (compendium/record-fact-existence! fact-function)
  fact-function)

(defn check-one [fact-function]
  (when (:midje/top-level-fact? (meta fact-function))
    (compendium/record-fact-check! fact-function))
  (#'midje.ideas.reporting.report/fact-begins)
  (within-runtime-fact-context (:midje/description (meta fact-function))
   (fact-function))
  (#'midje.ideas.reporting.report/fact-checks-out?))
    
