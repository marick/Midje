;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.background
  (:use
    [midje.util.form-utils :only [first-named? translate-zipper symbol-named? separate-by 
                                  pred-cond map-first]]
    [midje.util.exceptions :only [user-error]]
    [midje.ideas.metaconstants :only [define-metaconstants]]
    [midje.ideas.prerequisites :only [prerequisite-to-fake
                                      metaconstant-prerequisite?]]
    [midje.ideas.arrows :only [is-start-of-checking-arrow-sequence?
                               take-arrow-sequence]]
    [midje.util.laziness :only [eagerly]]
    [midje.internal-ideas.fakes :only [with-installed-fakes
                                       tag-as-background-fake
                                       fake?]]
    [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out with-pushed-namespace-values]]
    [midje.internal-ideas.wrapping :only [with-wrapping-target
                                          for-wrapping-target?]]
    [utilize.seq :only (separate)])
  (:require [midje.util.unify :as unify :only [bindings-map-or-nil ?form]]
            [clojure.zip :as zip]))

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
    [wrapping-target before-form & {:keys [after]}]
    (ensure-correct-form-variable `(try
                                     ~before-form
                                     ?form
                                     (finally ~after))))

  (defmacro after 
    "Code to run after a given wrapping target (:facts, :contents, :checks).
     Used with background and against-background"
    [wrapping-target after-form]
    (ensure-correct-form-variable `(try ?form (finally ~after-form))))

  (defmacro around 
    "Code to run around a given wrapping target (:facts, :contents, :checks).
     Use the symbol '?form' tp denote the code that is being wrapped around.
     
     ex.
     (around :contents (let [a 999] 
                         ?form
                         (print a))) 
     
     Used with background and against-background"
    [wrapping-target around-form]
    (ensure-correct-form-variable around-form)))

(defn seq-headed-by-setup-teardown-form? [forms]
  (when-let [bindings (setup-teardown-bindings (first forms))]
    (and (bindings '?first-form)
         (or (not (bindings '?after)) (bindings '?second-form)))))

(defn- prerequisites-to-fakes [forms]
  (loop [expanded []
         in-progress forms]
    (pred-cond in-progress
      empty? 
      expanded

      is-start-of-checking-arrow-sequence?
      (let [content (take-arrow-sequence in-progress)]
        (recur (conj expanded (-> content prerequisite-to-fake tag-as-background-fake))
          (nthnext in-progress (count content))))

      metaconstant-prerequisite?
      (let [content (take-arrow-sequence in-progress)]
        (recur (conj expanded (-> content prerequisite-to-fake))
          (nthnext in-progress (count content))))

      seq-headed-by-setup-teardown-form?
      (recur (conj expanded (first in-progress))
        (rest in-progress))

      :else (throw (user-error (str "This doesn't look like part of a background: "
                                 (vec in-progress)))))))

;; valid wrapping targets - :facts, :contents, or :checks

(defn- state-wrapper [[before-after-or-around wrapping-target & _  :as state-description]]
  (if (#{"before" "after" "around"} (name before-after-or-around))
    (with-wrapping-target
      (macroexpand-1 (map-first #(symbol "midje.ideas.background" (name %)) state-description))
      wrapping-target)
    (throw (user-error (str "Could make nothing of " state-description)))))

(letfn [(background-fake-wrappers [fakes]
          (let [around-facts-and-checks `(with-pushed-namespace-values
                                           :midje/background-fakes
                                           ~fakes ~(unify/?form))]
            (list 
             (with-wrapping-target around-facts-and-checks :facts))))]

  ;; Collecting all the background fakes is here for historical reasons:
  ;; it made it easier to eyeball expanded forms and see what was going on.
  (defn background-wrappers [background-forms]
    (define-metaconstants background-forms)
    (let [[fakes state-descriptions] (separate-by fake? (prerequisites-to-fakes background-forms))
          state-wrappers (eagerly (map state-wrapper state-descriptions))]
      (if (empty? fakes)
        state-wrappers
        (concat state-wrappers (background-fake-wrappers fakes))))))

(defn body-of-against-background [[_against-background_ background-forms & background-body]]
  `(do ~@background-body))

(defn against-background-contents-wrappers [[_against-background_ background-forms & _]]
  (filter (for-wrapping-target? :contents ) (background-wrappers background-forms)))

(defn against-background-children-wrappers [[_against-background_ background-forms & _]]
  (remove (for-wrapping-target? :contents ) (background-wrappers background-forms)))

(defn surround-with-background-fakes [forms]
  `(with-installed-fakes (background-fakes)
     (do ~@forms)))                                               