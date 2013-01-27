(ns ^{:doc "Parsing facts."}
  midje.parsing.1-to-explicit-form.facts
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        
        [midje.error-handling.validation-errors :only [validate when-valid]]

        [midje.parsing.1-to-explicit-form.expects :only [expect?
                                            wrap-with-expect__then__at-rightmost-expect-leaf]]
        [midje.parsing.util.file-position :only [annotate-embedded-arrows-with-line-numbers]]
        [midje.parsing.util.wrapping :only [already-wrapped?
                                              multiwrap
                                              with-additional-wrappers
                                              forms-to-wrap-around]]
        [midje.parsing.1-to-explicit-form.prerequisites :only [head-of-form-providing-prerequisites?
                                          insert-prerequisites-into-expect-form-as-fakes]]
        [midje.parsing.util.arrows :only [start-of-checking-arrow-sequence?]]
        [midje.parsing.1-to-explicit-form.background :only [surround-with-background-fakes
                                       body-of-against-background
                                       against-background-contents-wrappers
                                       against-background-facts-and-checks-wrappers
                                       against-background?]]
        [midje.parsing.1-to-explicit-form.metaconstants :only [predefine-metaconstants-from-form]]
        [midje.util.form-utils :only [def-many-methods
                                      pred-cond]]
        [midje.util.laziness :only [eagerly]]
        [midje.parsing.util.zip :only [skip-to-rightmost-leaf]]
        [swiss-arrows.core :only [-<>]])
  (:require [clojure.zip :as zip])
  (:require [midje.checking.facts :as fact-checking]
            [midje.data.compendium :as compendium]
            [midje.parsing.1-to-explicit-form.future-facts :as parse-future-facts]
            [midje.parsing.2-to-lexical-maps.folded-fakes :as parse-folded-fakes]
            [midje.data.fact :as fact-data]))

(defn fact? [form]
  (or (first-named? form "fact")
      (first-named? form "facts")))

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
    parse-future-facts/future-fact?         (macroexpand form)
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
      parse-folded-fakes/unfold-fakes
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

;; It's kind of annoying that the entire expansion is created, evaluated, 
;; and then thrown away. I think this is unavoidable if you want the
;; filter predicate to be applied in the repl.
(defn wrap-with-creation-time-code [function-form]
  (letfn [;; This form is a little awkward. It would be better to
          ;; write
          ;;     (if-let [fact ~function-form]
          ;;        (compendium/record-fact-existence! ...))
          ;; However, tabular expansion uses zip/seq-zip, so the
          ;; vector notation would confuse it.
          (wrap-with-creation-time-fact-recording [function-form]
            (if (working-on-top-level-fact?)
              `(compendium/record-fact-existence! ~function-form)
              function-form))
          
          (run-after-creation [function-form]
            `(fact-checking/creation-time-check ~function-form))]

    (predefine-metaconstants-from-form function-form)
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

