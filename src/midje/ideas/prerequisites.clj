(ns midje.ideas.prerequisites
  (:use [midje.util.namespace :only [matches-symbols-in-semi-sweet-or-sweet-ns?]]
        [midje.util.form-utils :only [symbol-named?]]
        [midje.internal-ideas.file-position :only [arrow-line-number-from-form]]
        [midje.ideas.metaconstants :only [metaconstant-for-form
                                          with-fresh-generated-metaconstant-names]]
        midje.ideas.arrow-symbols
        [midje.ideas.arrows :only [group-arrow-sequences]]
        [midje.internal-ideas.expect :only [up-to-full-expect-form
                                            tack-on__then__at-rightmost-expect-leaf]])
  (:require [clojure.zip :as zip]))

(defn is-head-of-form-providing-prerequisites? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(provided) loc))

(defn metaconstant-prerequisite? [[lhs arrow rhs & overrides :as fake-body]]
  (symbol-named? arrow =contains=>))

(defn prerequisite-to-fake [fake-body]
  (let [line-number (arrow-line-number-from-form fake-body)
        fake-tag (if (metaconstant-prerequisite? fake-body)
                   'midje.semi-sweet/data-fake
                   'midje.semi-sweet/fake)]
    (vary-meta
     `(~fake-tag ~@fake-body)
     assoc :line line-number)))

(defn expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (rest (zip/node (zip/up provided-loc)))
        fake-bodies (group-arrow-sequences fakes)]
    (map prerequisite-to-fake fake-bodies)))

(defn delete_prerequisite_form__then__at-previous-full-expect-form [loc]
  (assert (is-head-of-form-providing-prerequisites? loc))
  (let [x (-> loc zip/up zip/remove)]
    (up-to-full-expect-form x)))

(defn insert-prerequisites-into-expect-form-as-fakes [loc]
  (let [fake-calls (expand-prerequisites-into-fake-calls loc)
        full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
    (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form)))

