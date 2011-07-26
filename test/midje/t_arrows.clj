;; -*- indent-tabs-mode: nil -*-

(ns midje.t-arrows
  (:use midje.arrows)
  (:use [midje sweet test-util])
  (:require [clojure.zip :as zip]))

(fact "can ask if at first element of X =?> Y :possible :keywords"
  (let [possible (fn [nested-form] (zip/down (zip/seq-zip nested-form)))]
              "a string" =not=> is-start-of-arrow-sequence?
              '(foo) =not=> is-start-of-arrow-sequence?
    
              '( (f 1) ) =not=> is-start-of-arrow-sequence?
    (possible '( (f 1) )) =not=> is-start-of-arrow-sequence?
    
              '( (f 1) (f 2)) =not=> is-start-of-arrow-sequence?
    (possible '( (f 1) (f 2))) =not=> is-start-of-arrow-sequence?

              '( (f 1) => 2) => is-start-of-arrow-sequence?
    (possible '( (f 1) => 2)) => is-start-of-arrow-sequence?

              '( (f 1) =not=> 2) => is-start-of-arrow-sequence?
    (possible '( (f 1) =not=> 2)) => is-start-of-arrow-sequence?

              '( (f 1) => 2 :key 'value) => is-start-of-arrow-sequence?
    (possible '( (f 1) => 2 :key 'value)) => is-start-of-arrow-sequence?

              '( (f 1) midje.semi-sweet/=> 2) => is-start-of-arrow-sequence?
    (possible '( (f 1) midje.semi-sweet/=> 2)) => is-start-of-arrow-sequence?))

(fact "when at end of required part of arrow form, can ask for overrides"
    "empty rest of form"
    (arrow-sequence-overrides '()) => '()

    "new arrow form"
    (arrow-sequence-overrides '((g 1) => 1)) => '()

    "typical example of arrow-sequence-overrides"
    (arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33"))
    => '(:expected-result 3 :position "foo.clj:33")

    "Does not scoop up following forms"
    (arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33" (f 1)))
    => '(:expected-result 3 :position "foo.clj:33")

    "... even if those following forms have their own overrides"
    (arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33"
                                   (f 1) => 1 :expected-result 2))
    => '(:expected-result 3 :position "foo.clj:33"))


(facts "the run-on string of arrow forms can be grouped into a list of arrow sequences"
  ;; Use of let is to prevent #'fact from slapping a line number onto the results.
  (let [result (group-arrow-sequences '(   (f 1) => 2    (g 1) => 3))]
    result =>                         '(  [(f 1) => 2]  [(g 1) => 3] ))

  (let [result (group-arrow-sequences '(  (f 1) => 2 :key value   (g 1) => 3))]
    result =>                         '( [(f 1) => 2 :key value] [(g 1) => 3])))

