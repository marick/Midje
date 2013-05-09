(ns ^{:doc "Parsing facts."}
  midje.parsing.1-to-explicit-form.facts
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.arrow-symbols
        
        [midje.parsing.1-to-explicit-form.expects :only [wrap-with-expect__then__at-rightmost-expect-leaf]]
        [midje.parsing.1-to-explicit-form.prerequisites :only [insert-prerequisites-into-expect-form-as-fakes]]
        [midje.parsing.1-to-explicit-form.background :only [surround-with-background-fakes
                                                            body-of-against-background
                                                            against-background-contents-wrappers
                                                            against-background-facts-and-checks-wrappers
                                                            ]]
        [midje.parsing.1-to-explicit-form.metaconstants :only [predefine-metaconstants-from-form]]
        [midje.util.laziness :only [eagerly]])
  (:require [clojure.zip :as zip]
            [midje.parsing.util.zip :as pzip]
            [midje.parsing.util.overrides :as override]
            [midje.parsing.util.file-position :as position]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.1-to-explicit-form.background :as background]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.parsing.2-to-lexical-maps.expects :as parse-expects]
            [midje.parsing.2-to-lexical-maps.folded-fakes :as parse-folded-fakes]
            [midje.data.compendium :as compendium]
            [midje.checking.facts :as fact-checking]))

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
  (pzip/translate-zipper multi-form
    recognize/fact?
    pzip/skip-to-rightmost-leaf
                    
    recognize/start-of-checking-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf
    
    recognize/provided?
    insert-prerequisites-into-expect-form-as-fakes

    semi-sweet-keyword?
    pzip/skip-to-rightmost-leaf))

(declare midjcoexpand)

(defn expand-against-background [form]
  (background/assert-right-shape! form)
  (background/assert-contains-facts! form)
  (-<> form 
       body-of-against-background
       midjcoexpand
       (wrapping/with-additional-wrappers (against-background-facts-and-checks-wrappers form) <>)
       (wrapping/multiwrap <> (against-background-contents-wrappers form))))


(defn midjcoexpand
  "Descend form, macroexpanding *only* midje forms and placing background wrappers where appropriate."
  [form]
  (pred-cond form
    wrapping/already-wrapped?     form
    quoted?              form
    recognize/future-fact?         (macroexpand form)
    recognize/against-background?  (expand-against-background form)
    recognize/expect?      (wrapping/multiwrap form (wrapping/forms-to-wrap-around :checks ))
    recognize/fact?        (macroexpand form)
    sequential?  (preserve-type form (eagerly (map midjcoexpand form)))
    :else        form))

(defn parse-expects [form]
  (pzip/translate-zipper form
     recognize/expect? (fn [loc]
                         (zip/replace loc (parse-expects/to-lexical-map-form (zip/node loc))))))

(defn report-check-arrow-shape [form]
  (error/report-error form
                      (cl-format nil "    This form: ~A" form)
                      "... has the wrong shape. Expecting: (<actual> => <expected> [<keyword-value pairs>*])"))



(defn at-arrow__add-line-number-to-end__no-movement [number loc]
  (when (nil? (zip/right loc))
    (report-check-arrow-shape (position/positioned-form (zip/node (zip/up loc)) number)))
    
  (override/at-arrow__add-key-value-to-end__no-movement
   :position `(position/line-number-known ~number) loc))

(defn annotate-embedded-arrows-with-line-numbers [form]
  (pzip/translate-zipper form
    quoted?
    (comp pzip/skip-to-rightmost-leaf zip/down)

    (partial matches-symbols-in-semi-sweet-or-sweet-ns? recognize/all-arrows)
    #(at-arrow__add-line-number-to-end__no-movement (position/arrow-line-number %) %)))



(defn expand-fact-body [forms metadata]
  (-> forms
      annotate-embedded-arrows-with-line-numbers
      to-semi-sweet
      parse-folded-fakes/unfold-fakes
      surround-with-background-fakes
      midjcoexpand
      parse-expects
      (wrapping/multiwrap (wrapping/forms-to-wrap-around :facts))))


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

(defn complete-fact-transformation [metadata forms]
  (given-possible-fact-nesting
   (-> forms
       (expand-fact-body metadata)
       (wrap-with-check-time-code metadata)
       wrap-with-creation-time-code)))


(defn unparse-edited-fact
  "Uses a body and (parsed) metadata to make up a new `fact`.
   The resulting for has `:line` metadata. The :midje/source and
   :midje/guid are supplied explicitly in the
   new fact's metadata. That is, the final metadata will contain
   the source and guid of the original form."
  [metadata forms]
  (let [new-metadata (cons (select-keys metadata [:midje/source :midje/guid])
                           (parse-metadata/unparse-metadata metadata))]
    (vary-meta `(midje.sweet/fact ~@new-metadata ~@forms)
               assoc :line (:midje/line metadata))))
