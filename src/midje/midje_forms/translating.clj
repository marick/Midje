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
    [midje.midje-forms.editing :only [
                                      
                                      delete_prerequisite_form__then__at-previous-full-expect-form
                                      tack-on__then__at-rightmost-expect-leaf
                                      wrap-with-expect__then__at-rightmost-expect-leaf]]
    [midje.util.zip :only [skip-to-rightmost-leaf]]
    [midje.background :only [
                             background-form?]]
    [midje.semi-sweet :only [is-semi-sweet-keyword?]]
    [midje.util.wrapping :only [already-wrapped?]]
    [midje.fact :only [fact? future-fact?]]
    [midje.prerequisites :only [is-head-of-form-providing-prerequisites? mockable-funcall? folded-prerequisite?]]
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

(defn expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (rest (zip/node (zip/up provided-loc)))
        fake-bodies (group-arrow-sequences fakes)]
    (map make-fake fake-bodies)))

(defn translate-fact-body [multi-form]
  (translate multi-form
    is-start-of-arrow-sequence?
    wrap-with-expect__then__at-rightmost-expect-leaf
    
    is-head-of-form-providing-prerequisites?
    (fn [loc ] (let [fake-calls (expand-prerequisites-into-fake-calls loc)
                    full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
                 (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form)))

    is-semi-sweet-keyword?
    skip-to-rightmost-leaf))

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


;; Folded prerequisites

;; General strategy is to condense fake forms into a funcall=>metaconstant
;; mapping. These substitutions are used both to "flatten" a fake form and also
;; to generate new fakes.

(defn augment-substitutions [substitutions fake-form]
  (let [needed-keys (filter mockable-funcall?
                            (fake-form-funcall-arglist fake-form))]
    (reduce (fn [substitutions needed-key]
              ;; Note: because I like for a function's metaconstants to be
              ;; easily mappable to the original fake, I don't make one
              ;; unless I'm sure I need it.
              (if (get substitutions needed-key)
                substitutions
                (assoc substitutions needed-key (metaconstant-for-form needed-key))))
            substitutions
            needed-keys)))

(defn flatten-fake [ [fake [fun & args] & rest] substitutions]
  (let [new-args (map (fn [arg] (get substitutions arg arg)) args)]
    `(~fake (~fun ~@new-args) ~@rest)))

(defn generate-fakes [substitutions overrides]
  (map (fn [ [funcall metaconstant] ]
         `(midje.semi-sweet/fake ~funcall midje.semi-sweet/=> ~metaconstant ~@overrides))
       substitutions))

;; This walks through a `pending` list that may contain fakes. Each element is
;; copied to the `finished` list. If it is a suitable fake, its nested funcalls
;; are flattened (replaced with a metaconstant). If the metaconstant was newly
;; generated, the fake that describes it is added to the pending list. In that way,
;; it'll in turn be processed. This allows arbitrarily deep nesting.
(defn unfolding-step [finished pending substitutions]
  (let [target (first pending)]
    (if (folded-prerequisite? target)
      (let [overrides (nthnext target 4)
            augmented-substitutions (augment-substitutions substitutions target)
            flattened-target (flatten-fake target augmented-substitutions)
            generated-fakes (generate-fakes
                             (map-difference augmented-substitutions substitutions)
                             overrides)]
        [ (conj finished flattened-target)
          (concat generated-fakes (rest pending))
          augmented-substitutions])
    [(conj finished target), (rest pending), substitutions])))
  
(defn unfold-expect-form__then__stay_put [loc]
  (loop [ [finished pending substitutions] [ [] (zip/node loc) {} ]]
    (if (empty? pending)
      (zip/replace loc (apply list finished))
      (recur (unfolding-step finished pending substitutions)))))

(defn unfold-prerequisites [form]
  (with-fresh-generated-metaconstant-names
    (translate form
        expect?
        unfold-expect-form__then__stay_put)))


;; binding notes for tabular facts

