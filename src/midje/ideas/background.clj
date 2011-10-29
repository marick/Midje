;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.background
  (:use
    [midje.util.old-clojure-contrib.seq :only [separate]]
    [midje.util.form-utils :only [first-named? translate-zipper symbol-named? separate-by]]
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
                                          for-wrapping-target?]])
  (:require [midje.util.unify :as unify :only [bindings-map-or-nil ?form]]
            [clojure.zip :as zip]))

(defn against-background? [form]
  (first-named? form "against-background"))

(defn- ensure-correct-form-variable [form]
  (translate-zipper form
      (fn [loc] (symbol-named? (zip/node loc) "?form"))
      (fn [loc] (zip/replace loc (unify/?form)))))

(defn background-fakes []
  (namespace-values-inside-out :midje/background-fakes))

(defn background-fake-wrappers [fakes]
  (let [around-facts-and-checks `(with-pushed-namespace-values
                                   :midje/background-fakes
                                   ~fakes ~(unify/?form))]
    (list 
     (with-wrapping-target around-facts-and-checks :facts))))


;; dissecting background forms

(defn separate-background-forms [fact-forms]
  (let [[background-forms other-forms] (separate against-background? fact-forms)]
    [(mapcat rest background-forms) other-forms]))

(defn setup-teardown-bindings [form]
  (unify/bindings-map-or-nil form
                             '(?key ?when ?first-form ?after ?second-form)))


(defmacro before [wrapping-target before-form & [_ after-form & _ ] ]
  (ensure-correct-form-variable `(try
                                  ~before-form
                                  ?form
                                  (finally ~after-form))))

(defmacro after [wrapping-target after-form]
  (ensure-correct-form-variable `(try ?form (finally ~after-form))))

(defmacro around [wrapping-target around-form]
  (ensure-correct-form-variable around-form))

(defn seq-headed-by-setup-teardown-form? [forms]
  (when-let [bindings (setup-teardown-bindings (first forms))]
    (and (bindings '?first-form)
         (or (not (bindings '?after)) (bindings '?second-form)))))


(defn- prerequisites-to-fakes [forms]
  (loop [expanded []
         in-progress forms]
    (cond (empty? in-progress)
          expanded

          (is-start-of-checking-arrow-sequence? in-progress)
          (let [content (take-arrow-sequence in-progress)]
            (recur (conj expanded (-> content prerequisite-to-fake tag-as-background-fake))
                   (nthnext in-progress (count content))))

          (metaconstant-prerequisite? in-progress)
          (let [content (take-arrow-sequence in-progress)]
            (recur (conj expanded (-> content prerequisite-to-fake))
                   (nthnext in-progress (count content))))
          
          (seq-headed-by-setup-teardown-form? in-progress)
          (recur (conj expanded (first in-progress))
                 (rest in-progress))
          
          :else
          (throw (user-error (str "This doesn't look like part of a background: "
                                  (vec in-progress)))))))

(defn- state-wrapper [state-description]
  (if (some #{(name (first state-description))} '("before" "after" "around"))
    (with-wrapping-target
      (macroexpand-1 (cons (symbol "midje.ideas.background"
                                   (name (first state-description)))
                           (rest state-description)))
      (second state-description))
    (throw (user-error (str "Could make nothing of " state-description)))))

;; Collecting all the background fakes is here for historical reasons:
;; it made it easier to eyeball expanded forms and see what was going on.
(defn background-wrappers [background-forms]
  (define-metaconstants background-forms)
  (let [[fakes state-descriptions] (separate-by fake? (prerequisites-to-fakes background-forms))
        state-wrappers (eagerly (map state-wrapper state-descriptions))]
    (if (empty? fakes)
      state-wrappers
      (concat state-wrappers (background-fake-wrappers fakes)))))

(defn against-background-wrappers [against-background-form]
  (background-wrappers (second against-background-form)))

(defn against-background-body [form]
  `(do ~@(rest (rest form))))

(defn against-background-X-wrappers [filter-fun form]
  (filter-fun (for-wrapping-target? :contents) (against-background-wrappers form)))

(defn against-background-contents-wrappers [form]
  (against-background-X-wrappers filter form))

(defn against-background-children-wrappers [form]
  (remove (for-wrapping-target? :contents) (against-background-wrappers form)))


(defn surround-with-background-fakes [forms]
  `(with-installed-fakes (background-fakes)
     (do ~@forms)))



