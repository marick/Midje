;; -*- indent-tabs-mode: nil -*-

(ns midje.background
  (:use
    [clojure.contrib.seq :only [separate]]
    [midje.util.form-utils :only [form-first? translate symbol-named? separate-by]]
    [midje.metaconstants :only [define-metaconstants]]
    [midje.arrows :only [
                         is-start-of-arrow-sequence?
                         take-arrow-sequence
                         ]]
    [midje.util.laziness :only [eagerly]]
    [midje.fakes :only [
                        tag-as-background-fake
                        make-fake
                        fake?]]
    [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out with-pushed-namespace-values]]
    [midje.util.wrapping :only [with-wrapping-target]])
  (:require [midje.util.unify :as unify :only [bindings-map-or-nil ?form]]
            [clojure.zip :as zip]))

(defn background-form? [form] (form-first? form "against-background"))

(defn- ensure-correct-form-variable [form]
  (translate form       
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
  (let [[background-forms other-forms] (separate background-form? fact-forms)]
    [(mapcat rest background-forms) other-forms]))

(defn raw-wrappers [background-form] (second background-form))

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

          (is-start-of-arrow-sequence? in-progress)
          (let [content (take-arrow-sequence in-progress)]
            (recur (conj expanded (-> content make-fake tag-as-background-fake))
                   (nthnext in-progress (count content))))

          (seq-headed-by-setup-teardown-form? in-progress)
          (recur (conj expanded (first in-progress))
                 (rest in-progress))
          
          :else
          (throw (Error. (str "This doesn't look like part of a background: "
                              (vec in-progress)))))))

(defn- state-wrapper [state-description]
  (if (some #{(name (first state-description))} '("before" "after" "around"))
    (with-wrapping-target
      (macroexpand-1 (cons (symbol "midje.background"
                                   (name (first state-description)))
                           (rest state-description)))
      (second state-description))
    (throw (Error. (str "Could make nothing of " state-description)))))

;; Collecting all the background fakes is here for historical reasons:
;; it made it easier to eyeball expanded forms and see what was going on.
(defn background-wrappers [background-forms]
  (define-metaconstants background-forms)
  (let [[fakes state-descriptions] (separate-by fake? (prerequisites-to-fakes background-forms))
        state-wrappers (eagerly (map state-wrapper state-descriptions))]
    (if (empty? fakes)
      state-wrappers
      (concat state-wrappers (background-fake-wrappers fakes)))))


