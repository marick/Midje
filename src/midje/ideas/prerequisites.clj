(ns midje.ideas.prerequisites
  (:use [midje.util.namespace :only [namespacey-match]]
        [midje.internal-ideas.expect :only [expect? up-to-full-expect-form]]
        [midje.util.form-utils :only [translate
                                      map-difference
                                      ]]
        [midje.ideas.metaconstants :only [metaconstant-for-form
                                    with-fresh-generated-metaconstant-names]]
        [midje.ideas.arrows :only [group-arrow-sequences
                             ]]
        [midje.checkers.defining :only [checker-makers checker?]]
        [midje.internal-ideas.fakes :only [fake-form-funcall-arglist make-fake]])
  (:require [clojure.zip :as zip]))

(defn is-head-of-form-providing-prerequisites? [loc]
  (namespacey-match '(provided) loc))


;; Folded prerequisites

;; Note that folded prerequisites are in semi-sweet-style. (That is, they can only
;; be recognized after sweet style has been converted to semi-sweet.)

(def special-forms '[quote fn let new])

(defn- constructor? [symbol]
  (.endsWith (name symbol) "."))

(defn- mockable-function-symbol? [symbol]
  (not (or (some #{symbol} special-forms)
           (some #{symbol} checker-makers)
           (constructor? symbol)
           (checker? (resolve symbol)))))

(defn mockable-funcall? [thing]
  (and (list? thing)
       (mockable-function-symbol? (first thing))))

(defn folded-prerequisite? [form]
  (and (sequential? form)
       (= 'midje.semi-sweet/fake (first form))
       ;; We now know this: (fake (f ...arg... ...arg...) ...)
       (some mockable-funcall? (fake-form-funcall-arglist form))))

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



(defn expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (rest (zip/node (zip/up provided-loc)))
        fake-bodies (group-arrow-sequences fakes)]
    (map make-fake fake-bodies)))

(defn delete_prerequisite_form__then__at-previous-full-expect-form [loc]
  (assert (is-head-of-form-providing-prerequisites? loc))
  (let [x (-> loc zip/up zip/remove)]
    (up-to-full-expect-form x)))

