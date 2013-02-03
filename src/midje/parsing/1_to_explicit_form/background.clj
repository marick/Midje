(ns ^{:doc "Code to be run before, after or around facts. Also, 
            prerequisites that pertain to a group of facts."} 
  midje.parsing.1-to-explicit-form.background
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        [midje.parsing.util.arrows :only [start-of-checking-arrow-sequence? take-arrow-sequence]]
        [midje.parsing.1-to-explicit-form.metaconstants :only [predefine-metaconstants-from-form]]
        [midje.parsing.1-to-explicit-form.prerequisites :only [metaconstant-prerequisite? prerequisite-to-fake]]
        [midje.data.prerequisite-state :only [with-installed-fakes]]
        [midje.parsing.util.file-position :only [form-position]]
        [clojure.algo.monads :only [defmonad domonad]]
        [midje.parsing.util.wrapping :only [for-wrapping-target? with-wrapping-target]]
        [midje.util.laziness :only [eagerly]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out 
                                                   with-pushed-namespace-values]])
  (:require [clojure.zip :as zip]
            [midje.util.pile :as pile]
            [midje.util.unify :as unify]
            [midje.parsing.2-to-lexical-maps.fakes :as fakes]
            [midje.emission.api :as emit]))

(defn against-background? [form]
  (first-named? form "against-background"))

(defn background-fakes []
  (namespace-values-inside-out :midje/background-fakes))


;; dissecting background forms

(defn separate-background-forms [fact-forms]
  (let [[background-forms other-forms] (separate against-background? fact-forms)]
    [(mapcat rest background-forms) other-forms]))

(defn setup-teardown-bindings [form]
  (unify/bindings-map-or-nil form
                             '(?key ?when ?first-form ?after ?second-form)))

(letfn [(ensure-correct-form-variable [form]
          (translate-zipper form
            (fn [loc] (symbol-named? (zip/node loc) "?form"))
            (fn [loc] (zip/replace loc (unify/?form)))))]

  (defmacro before 
    "Code to run before a given wrapping target (:facts, :contents, :checks).
  Can take an optional keyword argument :after, for any code to run afterward.
  Used with background and against-background"
    [_wrapping-target_ before-form & {:keys [after]}]
    (ensure-correct-form-variable `(try
                                     ~before-form
                                     ?form
                                     (finally ~after))))

  (defmacro after 
    "Code to run after a given wrapping target (:facts, :contents, :checks).
  Used with background and against-background"
    [_wrapping-target_ after-form]
    (ensure-correct-form-variable `(try ?form (finally ~after-form))))

  (defmacro around 
    "Code to run around a given wrapping target (:facts, :contents, :checks).
  Use the symbol '?form' to denote the code that is being wrapped around.
     
  Ex.
  (around :contents (let [a 999] 
                      ?form
                      (print a))) 
     
  Used with background and against-background"
    [_wrapping-target_ around-form]
    (ensure-correct-form-variable around-form)))

(defn seq-headed-by-setup-teardown-form? [forms]
  (when-let [bindings (setup-teardown-bindings (first forms))]
    (and (bindings '?first-form)
         (or (not (bindings '?after)) (bindings '?second-form)))))

(defn- ^{:testable true } extract-state-descriptions+fakes [forms]
  (loop [expanded []
         in-progress forms]
    (pred-cond in-progress
      empty? 
      expanded

      start-of-checking-arrow-sequence?
      (let [arrow-seq (take-arrow-sequence in-progress)]
        (recur (conj expanded (-> arrow-seq prerequisite-to-fake fakes/tag-as-background-fake))
               (drop (count arrow-seq) in-progress)))

      metaconstant-prerequisite?
      (let [arrow-seq (take-arrow-sequence in-progress)]
        (recur (conj expanded (-> arrow-seq prerequisite-to-fake))
               (drop (count arrow-seq) in-progress)))

      seq-headed-by-setup-teardown-form?
      (recur (conj expanded (first in-progress))
             (rest in-progress)))))

(defn- ^{:testable true } state-wrapper [[_before-after-or-around_ wrapping-target & _ :as state-description]]
  (with-wrapping-target
    (macroexpand-1 (map-first #(symbol "midje.parsing.1-to-explicit-form.background" (name %)) state-description))
    wrapping-target))

(letfn [(background-fake-wrappers [fake-maker-forms]
          (let [around-facts-and-checks `(with-pushed-namespace-values
                                           :midje/background-fakes
                                           [~@fake-maker-forms] ~(unify/?form))]
            (list 
             (with-wrapping-target around-facts-and-checks :facts))))]

  ;; Collecting all the background fakes is here for historical reasons:
  ;; it made it easier to eyeball expanded forms and see what was going on.
  (defn background-wrappers [background-forms]
    (predefine-metaconstants-from-form background-forms)
    (let [[fakes state-descriptions] (separate fakes/fake? (extract-state-descriptions+fakes background-forms))
          state-wrappers (eagerly (map state-wrapper state-descriptions))]
      (if (empty? fakes)
        state-wrappers
        (concat state-wrappers (background-fake-wrappers fakes))))))

(defn body-of-against-background [[_against-background_ background-forms & background-body :as form]]
  `(do ~@background-body))

(defn against-background-contents-wrappers [[_against-background_ background-forms & _]]
  (filter (for-wrapping-target? :contents ) (background-wrappers background-forms)))

(defn against-background-facts-and-checks-wrappers [[_against-background_ background-forms & _]]
  (remove (for-wrapping-target? :contents ) (background-wrappers background-forms)))

(defn surround-with-background-fakes [forms]
  `(with-installed-fakes (background-fakes)
     ~@forms))


;;;;; Validation
;;;;; This is a lot of old complexity that's retained only because the idea of
;;;;; background needs to go away in favor of fact metadata




;; Making validation errors

(defn- ^{:testable true} as-validation-error [form]
  (vary-meta form assoc :midje/validation-error true))

(defn validation-error-form? [form]
  (:midje/validation-error (meta form)))

(defn validation-error-report-form [form & notes]
  (as-validation-error `(emit/fail {:type :parse-error
                                    :notes '~notes
                                    :position '~(form-position form)})))

(defn simple-validation-error-report-form [form & notes]
  (apply validation-error-report-form form (conj (vec notes) (pr-str form))))


;; Validation control flow macros

(defmonad validate-m
  "Monad describing form processing with possible failures. Failure
  is represented by any form with metadata :midje/validation-error"
  [m-result identity
   m-bind   (fn [form f] 
              (if (validation-error-form? form) form (f form)))  ])

(defmacro when-valid [validatable-form & body-to-execute-if-valid]
  `(domonad validate-m [_# (validate-old ~validatable-form)]
     ~@body-to-execute-if-valid))


;; Validate

(defmulti validate-old (fn [form & _options_] 
                     (if (named? (first form)) 
                       (name (first form)) 
                       :validate-seq)))

(defmethod validate-old :validate-seq [seq-of-forms & _options_]
  (let [first-validation-error (->> seq-of-forms 
                                    (map validate-old) 
                                    (filter validation-error-form?)
                                    first)]
    (or first-validation-error seq-of-forms)))

(defmethod validate-old :default [form & _options_] (rest form))

(def #^:private possible-wrapping-targets   #{:facts, :contents, :checks })
(def #^:private possible-state-descriptions #{"before" "after" "around"})

(pile/def-many-methods validate-old ["before" "after" "around"] [[state-description wrapping-target expression :as form]]
  (cond
    (and (#{"after" "around"} (name state-description)) (not= 3 (count form)))
    (validation-error-report-form form
      (cl-format nil "    In this form: ~A" form)
      (cl-format nil "~A forms should look like: (~A :contents/:facts/:checks (your-code))"
        (name state-description) (name state-description)))

    (and (= "before" (name state-description))
         (not= 3 (count form))
         (or (not= 5 (count form))
             (and (= 5 (count form))
                  (not= :after (nth form 3)))))
    (validation-error-report-form form
      (cl-format nil "    In this form: ~A" form)
      "before forms should look like: (before :contents/:facts/:checks (your-code)) or "
      "(before :contents/:facts/:checks (your-code) :after (final-code))")

    ((complement possible-wrapping-targets) wrapping-target)
    (validation-error-report-form form
      (cl-format nil "    In this form: ~A" form)
      (cl-format nil "The second element (~A) should be one of: :facts, :contents, or :checks"
        wrapping-target))

    :else (rest form)))

(letfn [(possible-state-descriptions+fakes? [forms]
          (loop [in-progress forms]
            (pred-cond in-progress
              empty?
              true

              (some-fn-m start-of-checking-arrow-sequence? metaconstant-prerequisite?)
              (let [arrow-seq (take-arrow-sequence in-progress)]
                (recur (drop (count arrow-seq) in-progress)))

              seq-headed-by-setup-teardown-form?
              (recur (rest in-progress))

              :else false)))

        (state-description? [form]
          (and (sequential? form)
            (contains? possible-state-descriptions (name (first form))))) ]

  (defmethod validate-old "against-background" 
    [[_against-background_ state-descriptions+fakes & _body_ :as form]]
    (cond (< (count form) 3)
          (simple-validation-error-report-form form
            "You need a minimum of three elements to an against-background form:")
       
          (vector? state-descriptions+fakes)                                    
          (when-valid (filter state-description? state-descriptions+fakes)
            (pred-cond state-descriptions+fakes   
              empty?
              (simple-validation-error-report-form form
                "You put nothing in the background:")
            
              (comp not possible-state-descriptions+fakes?)
              (simple-validation-error-report-form form
                "Badly formatted background prerequisites:")
        
              :else
              (rest form)))
            
          (sequential? state-descriptions+fakes)
          (if (named? (first state-descriptions+fakes))
            (when-valid state-descriptions+fakes (rest form))
            (rest form))
            
          :else                                      
          (simple-validation-error-report-form form 
            "Malformed against-background. against-background requires"
            "at least one background fake or background wrapper: " )))
  
  (defmethod validate-old "background" [[_background_ & state-descriptions+fakes :as form]]
    (when-valid (filter state-description? state-descriptions+fakes) 
      (pred-cond state-descriptions+fakes 
        empty?
        (simple-validation-error-report-form form "You put nothing in the background:")
    
        (comp not possible-state-descriptions+fakes?)
        (simple-validation-error-report-form form "Badly formatted background prerequisites:")
        
        :else
        state-descriptions+fakes))))



