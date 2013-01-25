(ns ^{:doc "Functions for turning provideds into semi-sweet fakes"}
  midje.parsing.1-to-explicit-form.prerequisites
  (:use [midje.util.namespace :only [matches-symbols-in-semi-sweet-or-sweet-ns?]]
        [midje.util.form-utils :only [symbol-named?]]
        [midje.parsing.util.file-position :only [arrow-line-number-from-form]]
        midje.parsing.arrow-symbols
        [midje.parsing.util.arrows :only [pull-all-arrow-seqs-from]]
        [midje.parsing.1-to-explicit-form.expects :only [up-to-full-expect-form expect?
                                            tack-on__then__at-rightmost-expect-leaf]])
  (:require [clojure.zip :as zip]
            [midje.util.ecosystem :as ecosystem]))

(defn head-of-form-providing-prerequisites? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(provided) loc))

(defn metaconstant-prerequisite? [[lhs arrow rhs & overrides :as fake-body]]
  (symbol-named? arrow =contains=>))

(defn prerequisite-to-fake [fake-body]
  (let [^Integer line-number (arrow-line-number-from-form fake-body)
        fake-tag (if (metaconstant-prerequisite? fake-body)
                   'midje.semi-sweet/data-fake
                   'midje.semi-sweet/fake)]
    (vary-meta
     `(~fake-tag ~@fake-body)
     assoc :line (Integer. line-number))))

(defn expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (-> provided-loc zip/up zip/node rest)
        fake-bodies (pull-all-arrow-seqs-from fakes)]
    (map prerequisite-to-fake fake-bodies)))

(defn delete_prerequisite_form__then__at-previous-full-expect-form [loc]
  (assert (head-of-form-providing-prerequisites? loc))
  (-> loc zip/up zip/remove up-to-full-expect-form))

(defn- previous-loc [loc]
  (zip/left (zip/up loc)))

(defn- following-expect? [loc]
  (expect? (previous-loc loc)))

(defn- previous-form [loc]
  (zip/node (previous-loc loc)))
  

(defn insert-prerequisites-into-expect-form-as-fakes [loc]
  (if (following-expect? loc)
    (let [fake-calls (expand-prerequisites-into-fake-calls loc)
          full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
      (tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form))
    (throw (Error. (str ecosystem/line-separator
                        "The form before the `provided` is not a check:"
                        ecosystem/line-separator
                        (pr-str (previous-form loc))
                        ecosystem/line-separator
                        "Here are common errors when writing a form like the following:"
                        ecosystem/line-separator
                        "   (f ..arg..) => 0"
                        ecosystem/line-separator
                        "   (provided"
                        ecosystem/line-separator
                        "     ...)"
                        ecosystem/line-separator
                        "Misparenthesization: `(f ..arg.. => 0) (provided... `"
                        ecosystem/line-separator
                        "Missing =>: `(f ..arg..) (provided...` "
                        ecosystem/line-separator
                        ecosystem/line-separator
                        )))))
