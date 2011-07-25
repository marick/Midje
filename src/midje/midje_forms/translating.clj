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
                         above_arrow_sequence__add-key-value__at_arrow
                         at_arrow__add-line-number-to-end__no-movement]]
    [midje.metaconstants :only [define-metaconstants]]
    [midje.fakes :only [
                        tag-as-background-fake
                        make-fake
                        fake?]]
    [midje.metaconstants :only [metaconstant-for-form
                                       with-fresh-generated-metaconstant-names]]
    [midje.background :only [background-fake-wrappers
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
    [midje.midje-forms.recognizing :only [
				      fact?
				      fake-form-funcall-arglist
				      fake-that-needs-unfolding?
				      future-fact?
				      is-head-of-form-providing-prerequisites?
				      mockable-funcall?
				      seq-headed-by-setup-teardown-form?]]
    [midje.util.debugging :only [nopret]]
    [midje.util.file-position :only [arrow-line-number]]
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

(defn interior-forms [form]
  `(do ~@(rest (rest form))))


;; Translating a form into an equivalent form with all arrow sequences given
;; line numbers. 

(defn add-line-numbers [form]
  (translate form
    (fn [loc] (namespacey-match all-arrows loc))
    (fn [loc] (at_arrow__add-line-number-to-end__no-movement (arrow-line-number loc) loc))))

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

(declare midjcoexpand)

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

(defn forms-to-wrap-around [wrapping-target]
  (filter (for-wrapping-target? wrapping-target) (wrappers)))

(defn midjcoexpand [form]
  ;; (p+ "== midjcoexpanding" form)
  ;; (p "== with" (wrappers))
  (nopret (cond (already-wrapped? form)
        form

        (form-first? form "quote")
        form

        (future-fact? form)
        (macroexpand form)

        (expect? form)
        (multiwrap form (forms-to-wrap-around :checks))

        (fact? form)
        (do
          (multiwrap (midjcoexpand (macroexpand form))
                     (forms-to-wrap-around :facts)))

        (background-form? form)
        (do
          ;; (p+ "use these wrappers" (raw-wrappers form))
          ;; (p "for this form" (interior-forms form))
          ;; (p (wrappers))
          (nopret (let [wrappers (final-wrappers (raw-wrappers form))
                      [now-wrappers later-wrappers] (separate (for-wrapping-target? :contents)
                                                              wrappers)]
            ;; "Now wrappers" have to be separated out and discarded here, because
            ;; if they were left in, they'd be reapplied in any nested background
            ;; forms.
            ;; (p "now-wrappers" now-wrappers)
            ;; (p "later-wrappers" later-wrappers)
            (multiwrap (with-additional-wrappers later-wrappers
                          (midjcoexpand (interior-forms form)))
                       now-wrappers))))
        
        (sequential? form)
        (preserve-type form (eagerly (map midjcoexpand form)))

        :else
        form)))

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
    (if (fake-that-needs-unfolding? target)
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

(defn- replace-loc-line [loc loc-with-line]
  (let [m (fn [loc] (meta (zip/node loc)))
        transferred-meta (if (contains? (m loc-with-line) :line)
                           (assoc (m loc) :line (:line (m loc-with-line)))
                           (dissoc (m loc) :line))]
    (zip/replace loc (with-meta (zip/node loc) transferred-meta))))

(defn form-with-copied-line-numbers [form line-number-source]
  (loop [loc (zip/seq-zip form)
         line-loc (zip/seq-zip line-number-source)]
    (cond (zip/end? line-loc)
          (zip/root loc)

          (zip/branch? line-loc)
          (recur (zip/next (replace-loc-line loc line-loc))
                 (zip/next line-loc))

          ;; the form has a tree in place of a non-tree
          (zip/branch? loc)
            (recur (zip/next
                    (skip-to-rightmost-leaf (zip/down (replace-loc-line loc line-loc))))
                   (zip/next line-loc))

          :else
          (recur (zip/next loc)
                 (zip/next line-loc)))))

;; binding notes for tabular facts

(defn- binding-note [ordered-binding-map]
  (let [entries (map (fn [[variable value]] (str variable " " (pr-str value))) ordered-binding-map)]
    (str "{" (str-join ", " entries) "}")))

(defn add-one-binding-note [expect-containing-form ordered-binding-map]
  (translate expect-containing-form
    expect?
    (fn [loc] (skip-to-rightmost-leaf
      (above_arrow_sequence__add-key-value__at_arrow :binding-note (binding-note ordered-binding-map) loc)))))

(defn add-binding-notes [expect-containing-forms ordered-binding-maps]
  (map (partial apply add-one-binding-note) 
       (pairs expect-containing-forms ordered-binding-maps)))
