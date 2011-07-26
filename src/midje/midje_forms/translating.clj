;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.translating
  (:use
    [clojure.contrib.seq :only [separate]]
    [clojure.contrib.str-utils :only [str-join]]
    [midje.util.namespace :only [namespacey-match]]
    [midje.expect :only [expect?]]
    [midje.arrows :only [all-arrows
                         is-start-of-arrow-sequence?
                         take-arrow-sequence
                         group-arrow-sequences
                         ]]
    [midje.metaconstants :only [define-metaconstants]]
    [midje.fakes :only [
                        tag-as-background-fake
                        fake-form-funcall-arglist
                        make-fake
                        fake?]]
    [midje.metaconstants :only [metaconstant-for-form
                                       with-fresh-generated-metaconstant-names]]
    [midje.background :only [background-fake-wrappers
                             seq-headed-by-setup-teardown-form?
                             setup-teardown-bindings
                                         raw-wrappers]]
    [midje.util.zip :only [skip-to-rightmost-leaf]]
    [midje.background :only [
                             background-form?]]
    [midje.semi-sweet :only [is-semi-sweet-keyword?]]
    [midje.util.wrapping :only [already-wrapped?]]
    [midje.fact :only [fact? future-fact?]]
    [midje.prerequisites :only [is-head-of-form-providing-prerequisites? mockable-funcall? folded-prerequisite?
                                expand-prerequisites-into-fake-calls]]
    [midje.util.debugging :only [nopret]]
    [midje.internal-ideas.file-position :only [arrow-line-number]]
    [midje.util.form-utils :only [form-first?
				      map-difference
				      pairs
				      preserve-type
				      separate-by
				      translate]]
    [midje.util.laziness :only [eagerly]]
    [midje.util.wrapping :only [for-wrapping-target?
				      multiwrap
				      set-wrappers
				      with-additional-wrappers
				      with-wrapping-target
				      wrappers]])
  (:require [clojure.zip :as zip]))


;; Translating a form into an equivalent form with all arrow sequences given
;; line numbers. 

;; Translating sweet forms into their semi-sweet equivalent

;; There are three variants of background forms, here referred to as "wrappers":
;; 1. RAW - wrappers mixed up, like [ (f 1) => 3 (before ...) (f 2) => 3) ]. Needs parsing.
;; 2. CANONICALIZED - one form per wrapper, perhaps some transformation.
;; 3. FINAL - a nesting form that can be unified with included forms.

(defn- canonicalize-raw-wrappers [forms]
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

(defn- final-state-wrapper [canonicalized-non-fake]
  (if (some #{(name (first canonicalized-non-fake))} '("before" "after" "around"))
    (with-wrapping-target
      (macroexpand-1 (cons (symbol "midje.background"
                                   (name (first canonicalized-non-fake)))
                           (rest canonicalized-non-fake)))
      (second canonicalized-non-fake))
    (throw (Error. (str "Could make nothing of " canonicalized-non-fake)))))

;; Collecting all the background fakes is here for historical reasons:
;; it made it easier to eyeball expanded forms and see what was going on.
(defn final-wrappers [raw-wrappers]
  (define-metaconstants raw-wrappers)
  (let [canonicalized (canonicalize-raw-wrappers raw-wrappers)
        [fakes state-wrappers] (separate-by fake? canonicalized)
        final-state-wrappers (eagerly (map final-state-wrapper state-wrappers))]
    (if (empty? fakes)
      final-state-wrappers
      (concat final-state-wrappers (background-fake-wrappers fakes)))))

(defn put-wrappers-into-effect [raw-wrappers]
  (let [[immediates finals] (separate (for-wrapping-target? :contents)
                                      (final-wrappers raw-wrappers))]
    (set-wrappers finals)
    (multiwrap "unimportant-value" immediates)))


