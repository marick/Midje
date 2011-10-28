;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-arrows
  (:use midje.ideas.arrows)
  (:use [midje sweet test-util])
  (:require [clojure.zip :as zip]))

(fact "can ask if at first element of X =?> Y :possible :keywords"
  (let [possible (fn [nested-form] (zip/down (zip/seq-zip nested-form)))]
              "a string" =not=> is-start-of-checking-arrow-sequence?
              '(foo) =not=> is-start-of-checking-arrow-sequence?
    
              '( (f 1) ) =not=> is-start-of-checking-arrow-sequence?
    (possible '( (f 1) )) =not=> is-start-of-checking-arrow-sequence?
    
              '( (f 1) (f 2)) =not=> is-start-of-checking-arrow-sequence?
    (possible '( (f 1) (f 2))) =not=> is-start-of-checking-arrow-sequence?

              '( (f 1) => 2) => is-start-of-checking-arrow-sequence?
    (possible '( (f 1) => 2)) => is-start-of-checking-arrow-sequence?

              '( (f 1) =not=> 2) => is-start-of-checking-arrow-sequence?
    (possible '( (f 1) =not=> 2)) => is-start-of-checking-arrow-sequence?

              '( (f 1) => 2 :key 'value) => is-start-of-checking-arrow-sequence?
    (possible '( (f 1) => 2 :key 'value)) => is-start-of-checking-arrow-sequence?

              '( (f 1) midje.semi-sweet/=> 2) => is-start-of-checking-arrow-sequence?
    (possible '( (f 1) midje.semi-sweet/=> 2)) => is-start-of-checking-arrow-sequence?))

(fact "when at end of required part of arrow form, can ask for overrides and the
       length of the original form taken to extract those overrides"
    "empty rest of form"
    (take-arrow-sequence-overrides '()) => ['(), 0]

    "new arrow form"
    (take-arrow-sequence-overrides '((g 1) => 1)) => ['(), 0]

    "typical example of take-arrow-sequence-overrides"
    (take-arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33"))
    => ['(:expected-result 3 :position "foo.clj:33"), 4]

    "Does not scoop up following forms"
    (take-arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33" (f 1)))
    => ['(:expected-result 3 :position "foo.clj:33"), 4]

    "Does not scoop up following forms - when using :never syntactic sugar; also,
     takes 3 from forms to extract the overrides of length 4"
    (take-arrow-sequence-overrides '( :expected-result 3 :never (f 1)))
    => ['(:expected-result 3 :times 0), 3]

    "... even if those following forms have their own overrides"
    (take-arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33"
                                   (f 1) => 1 :expected-result 2))
    => ['(:expected-result 3 :position "foo.clj:33"), 4])


(facts "the run-on string of arrow forms can be grouped into a list of arrow sequences"
  ;; Use of let is to prevent #'fact from slapping a line number onto the results.
  (let [result (parse-prerequisites-arrow-seqs '(   (f 1) => 2    (g 1) => 3))]
    result =>                                  '(  [(f 1) => 2]  [(g 1) => 3] ))

  (let [result (parse-prerequisites-arrow-seqs '(  (f 1) => 2 :key value   (g 1) => 3))]
    result =>                                  '( [(f 1) => 2 :key value] [(g 1) => 3]))

  (let [result (parse-prerequisites-arrow-seqs '(  (f 1) => 2 :never     (g 1) => 3))]
    result =>                                  '( [(f 1) => 2 :times 0] [(g 1) => 3])))

