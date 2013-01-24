(ns ^{:doc "Code to be run before, after or around facts. Also, 
            prerequisites that pertain to a group of facts."} 
  midje.parsing.1-to-normal-form.background
  (:use [midje.ideas.arrows :only [start-of-checking-arrow-sequence? take-arrow-sequence]]
        [midje.parsing.1-to-normal-form.metaconstants :only [predefine-metaconstants-from-form]]
        [midje.parsing.1-to-normal-form.prerequisites :only [metaconstant-prerequisite? prerequisite-to-fake]]
        [midje.internal-ideas.fakes :only [with-installed-fakes]]
        [midje.internal-ideas.wrapping :only [for-wrapping-target? with-wrapping-target]]
        [midje.util.form-utils :only [first-named? map-first pred-cond separate-by
                                      symbol-named? translate-zipper]]
        [midje.util.laziness :only [eagerly]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out 
                                                   with-pushed-namespace-values]]
        [utilize.seq :only [separate]])
  (:require [clojure.zip :as zip] 
            [midje.util.unify :as unify]
            [midje.parsing.2-to-lexical-maps.fakes :as fakes]))

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
    (macroexpand-1 (map-first #(symbol "midje.parsing.1-to-normal-form.background" (name %)) state-description))
    wrapping-target))

(letfn [(background-fake-wrappers [fakes]
          (let [around-facts-and-checks `(with-pushed-namespace-values
                                           :midje/background-fakes
                                           ~fakes ~(unify/?form))]
            (list 
             (with-wrapping-target around-facts-and-checks :facts))))]

  ;; Collecting all the background fakes is here for historical reasons:
  ;; it made it easier to eyeball expanded forms and see what was going on.
  (defn background-wrappers [background-forms]
    (predefine-metaconstants-from-form background-forms)
    (let [[fakes state-descriptions] (separate-by fakes/fake? (extract-state-descriptions+fakes background-forms))
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
