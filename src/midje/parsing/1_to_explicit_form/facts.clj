(ns ^{:doc "Parsing facts."}
  midje.parsing.1-to-explicit-form.facts
  (:require [clojure.zip :as zip]
            [such.control-flow :refer [branch-on]]
            [clojure.pprint :as pprint]
            [midje.data.compendium :as compendium]
            [midje.parsing.1-to-explicit-form.expects :refer [wrap-with-expect__then__at-rightmost-expect-leaf]]
            [midje.parsing.1-to-explicit-form.metaconstants :refer [predefine-metaconstants-from-form]]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.1-to-explicit-form.parse-background :as parse-background]
            [midje.parsing.1-to-explicit-form.prerequisites :refer [insert-prerequisites-into-expect-form-as-fakes]]
            [midje.parsing.2-to-lexical-maps.expects :as parse-expects]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.parsing.2-to-lexical-maps.folded-fakes :as parse-folded-fakes]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.expanded-symbols :as expanded-symbols]
            [midje.parsing.util.core :refer :all]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.overrides :as override]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.util.zip :as pzip]
            [midje.util.laziness :refer [eagerly]]
            [pointer.core :as pointer]
            [midje.checking.facts :as fact-checking]))

                                ;;; Fact processing

;; There are three stages to fact processing:
;; * Body processing: the convertion of arrow and provided forms into their
;;   explicit forms, the insertion of background data, line numbering, etc.
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

(defn already-expanded? [loc]
  (expanded-symbols/all (zip/node loc)))

(defn prerequisite-arrow-out-of-place [loc]
  (let [arrow      (-> loc zip/right zip/node)
        left-expr  (-> loc zip/node)
        right-expr (-> loc zip/right zip/right zip/node)]
  (error/report-error left-expr
                      "The prerequisite arrow appears outside the body of a `provided`:"
                      (str left-expr " " arrow " " right-expr))))

(defn- to-explicit-form
  "Convert sweet pseudo-forms into their explicit equivalents.
   1) Arrow sequences become expect forms.
   2) (provided ...) become fakes inserted into preceding expect."
  [multi-form]
  (pzip/translate-zipper multi-form
    recognize/fact?
    pzip/skip-to-rightmost-leaf

    recognize/start-of-checking-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf

    recognize/start-of-prerequisite-arrow-sequence?
    prerequisite-arrow-out-of-place

    recognize/provided?
    insert-prerequisites-into-expect-form-as-fakes

    already-expanded?
    pzip/skip-to-rightmost-leaf))

(declare midjcoexpand)

(defn expand-wrapping-background-changer [form]
  (parse-background/assert-right-shape! form)
  (parse-background/assert-contains-facts! form)
  (->> form
       parse-background/body-of-against-background
       midjcoexpand
       (wrapping/with-additional-wrappers (parse-background/against-background-facts-and-checks-wrappers form))
       (#(wrapping/multiwrap % (parse-background/against-background-contents-wrappers form)))))

;; Note that this predicate assumes that extractable (non-wrapping) background changers
;; have already been extracted from the body of a fact.
(defn- wrapping-background-changer? [form]
  (or (first-named? form "against-background")
      (first-named? form "with-state-changes")))

(defn midjcoexpand
  "Descend form, macroexpanding *only* midje forms and placing background
  wrappers where appropriate."
  [form]
  (branch-on form
    wrapping/already-wrapped?      form
    quoted?                        form
    recognize/future-fact?         (macroexpand form)
    ;; The `prerequisites` form is not supposed to be used in wrapping style.
    wrapping-background-changer?  (expand-wrapping-background-changer form)
    recognize/expect?             (wrapping/multiwrap
                                    form (wrapping/forms-to-wrap-around :checks))
    recognize/fact?               (macroexpand form)
    recognize/tabular?            (macroexpand form)
    recognize/for-all?            (macroexpand form)
    sequential?                   (preserve-type form (eagerly (map midjcoexpand form)))
    :else                         form))

(defn parse-expects [form]
  (pzip/translate-zipper form
     recognize/expect? (fn [loc]
                         (zip/replace loc (parse-expects/to-lexical-map-form (zip/node loc))))))

(defn report-check-arrow-shape [form]
  (error/report-error form
                      (pprint/cl-format nil "    This form: ~A" form)
                      "... has the wrong shape. Expecting: (<actual> => <expected> [<keyword-value pairs>*])"))



(defn- at-arrow__add-line-number-to-end__no-movement [number loc]
  (when (nil? (zip/right loc))
    (report-check-arrow-shape (pointer/positioned-form (zip/node (zip/up loc)) number)))

  (override/at-arrow__add-key-value-to-end__no-movement
   :position `(pointer/line-number-known ~number) loc))

(defn annotate-embedded-arrows-with-line-numbers [form]
  (pzip/translate-zipper form
    quoted?
    (comp pzip/skip-to-rightmost-leaf zip/down)

    recognize/any-arrow?
    #(at-arrow__add-line-number-to-end__no-movement (pointer/line-number-for %) %)))

(defn- expand-fact-body [forms]
  (-> forms
      annotate-embedded-arrows-with-line-numbers
      to-explicit-form
      parse-folded-fakes/unfold-fakes
      parse-background/surround-with-background-fakes
      midjcoexpand
      parse-expects
      (wrapping/multiwrap (wrapping/forms-to-wrap-around :facts))))


;;; Metadata handling

(defn add-metadata [expanded-body metadata]
  `(with-meta (fn [] ~expanded-body) (merge ~metadata
                                            {:midje/top-level-fact? ~(working-on-top-level-fact?)})))

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
       expand-fact-body
       (add-metadata metadata)
       wrap-with-creation-time-code)))

(defn wrap-fact-around-body
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
