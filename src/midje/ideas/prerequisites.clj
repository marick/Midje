(ns midje.ideas.prerequisites
  (:use [midje.util.namespace :only [namespacey-match]]
        [midje.ideas.metaconstants :only [metaconstant-for-form
                                          with-fresh-generated-metaconstant-names]]
        [midje.ideas.arrows :only [group-arrow-sequences]]
        [midje.internal-ideas.expect :only [up-to-full-expect-form
                                            tack-on__then__at-rightmost-expect-leaf]]
        [midje.internal-ideas.fakes :only [fake-form-funcall-arglist make-fake]])
  (:require [clojure.zip :as zip]))

(defn is-head-of-form-providing-prerequisites? [loc]
  (namespacey-match '(provided) loc))




(defn expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (rest (zip/node (zip/up provided-loc)))
        fake-bodies (group-arrow-sequences fakes)]
    (map make-fake fake-bodies)))

(defn delete_prerequisite_form__then__at-previous-full-expect-form [loc]
  (assert (is-head-of-form-providing-prerequisites? loc))
  (let [x (-> loc zip/up zip/remove)]
    (up-to-full-expect-form x)))

(defn insert-prerequisites-into-expect-form-as-fakes [loc]
  (let [fake-calls (expand-prerequisites-into-fake-calls loc)
        full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
    (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form)))

