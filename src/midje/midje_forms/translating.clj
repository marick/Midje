;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.translating
  (:use clojure.contrib.def
        [clojure.contrib.seq :only [separate]]
        midje.metaconstants
        [midje.semi-sweet :only [all-arrows]]
        [midje.util thread-safe-var-nesting wrapping form-utils laziness form-utils]
        [midje.util.file-position :only [arrow-line-number]]
        [midje.midje-forms building recognizing dissecting moving-around editing]
        [midje.fakes :only [background-fake-wrappers]]
        [midje.util.debugging])
  (:require [clojure.zip :as zip]))

;; Translating a form into an equivalent form with all arrow sequences given
;; line numbers. 

(defn add-line-numbers [form]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond (namespacey-match all-arrows loc)
                             (add-line-number-to-end-of-arrow-sequence__then__no-movement
                               (arrow-line-number loc) loc)
                             
                             :else loc))))))



;; Translating sweet forms into their semi-sweet equivalent

(defn expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (rest (zip/node (zip/up provided-loc)))
        fake-bodies (partition-arrow-forms fakes)]
    (map make-fake fake-bodies)))

(defn translate-fact-body [multi-form]
  (loop [loc (zip/seq-zip multi-form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur
       (zip/next
        (cond (loc-is-start-of-check-sequence? loc)
              (wrap-with-expect__then__at-rightmost-expect-leaf loc)

              (loc-is-head-of-form-providing-prerequisites? loc)
              (let [fake-calls (expand-prerequisites-into-fake-calls loc)
                    full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
                (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form))

              (loc-is-semi-sweet-keyword? loc)
              (skip-to-rightmost-leaf loc)

              :else loc))))))



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

          (is-arrow-form? in-progress)
          (let [content (take-arrow-form in-progress)]
            (recur (conj expanded (-> content make-fake make-background))
                   (nthnext in-progress (count content))))

          (seq-headed-by-setup-teardown-form? in-progress)
          (recur (conj expanded (first in-progress))
                 (rest in-progress))
          
          :else
          (throw (Error. (str "This doesn't look like part of a background: "
                              (vec in-progress)))))))

(defn- final-state-wrapper [canonicalized-non-fake]
;  (println canonicalized-non-fake)
  (if (some #{(name (first canonicalized-non-fake))} '("before" "after" "around"))
    (with-wrapping-target
      (macroexpand-1 (cons (symbol "midje.midje-forms.building"
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
  (with-fresh-generated-metadata-names
    (loop [loc (zip/seq-zip form)]
      (if (zip/end? loc)
        (zip/root loc)
        (recur (zip/next (cond (loc-is-at-full-expect-form? loc)
                               (unfold-expect-form__then__stay_put loc)

                               :else loc)))))))

