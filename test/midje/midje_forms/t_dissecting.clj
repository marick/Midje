;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-dissecting
  (:require [clojure.zip :as zip])
  (:use [midje.midje-forms dissecting recognizing])
  (:use midje.sweet)
  (:use midje.test-util)
)

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-background-forms '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])


(fact "separate-background-forms divides forms into background and other things"
  (separate-background-forms []) =>
                 [ [] [] ]
  (separate-background-forms '[ (f 1) => 3 ]) =>
             [ [] '[ (f 1) => 3 ] ]
  (separate-background-forms '[ (against-background) (f 1) => 3 ]) =>
                                 [ [] '[ (f 1) => 3 ] ]
  (separate-background-forms '[ (against-background (g) => 22)     (f 1) => 3 ]) =>
                                    [ '[(g) => 22] '[ (f 1) => 3 ] ]
  (separate-background-forms '[ (against-background (g) => 22)
                    (f 1) => 3
                    (against-background (h) => 3)]) => [ '[(g) => 22 (h) => 3]
                                                         '[ (f 1) => 3 ] ])


(fact "when at end of required part of arrow form, can ask for overrides"
    "empty rest of form"
    (arrow-form-overrides '()) => '()

    "new arrow form"
    (arrow-form-overrides '((g 1) => 1)) => '()

    "typical example of arrow-form-overrides"
    (arrow-form-overrides '( :expected-result 3 :file-position "foo.clj:33"))
    => '(:expected-result 3 :file-position "foo.clj:33")

    "Does not scoop up following forms"
    (arrow-form-overrides '( :expected-result 3 :file-position "foo.clj:33" (f 1)))
    => '(:expected-result 3 :file-position "foo.clj:33")

    "... even if those following forms have their own overrides"
    (arrow-form-overrides '( :expected-result 3 :file-position "foo.clj:33"
                                   (f 1) => 1 :expected-result 2))
    => '(:expected-result 3 :file-position "foo.clj:33"))

(facts "the run-on string of arrow forms can be partitioned"
  ;; Use of let is to prevent #'fact from slapping a line number onto the results.
  (let [result (partition-arrow-forms '(   (f 1) => 2    (g 1) => 3))]
    result =>                         '(  [(f 1) => 2]  [(g 1) => 3] ))

  (let [result (partition-arrow-forms '(  (f 1) => 2 :key value   (g 1) => 3))]
    result =>                         '( [(f 1) => 2 :key value] [(g 1) => 3])))



(defn at-line [line-no form] (with-meta form {:line line-no}))

(facts "about determining an arrow sequences line number from nearby forms"

  "Typical case is form on left. (f 1) => 5"
  (let [form `( ~(at-line 33 '(f 1)) => 5)
        loc (-> form zip/seq-zip zip/down)]
    loc => loc-is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "When form on the left is has no line, check right: ...a... => (exactly 1)"
  (let [form `( ...a... => ~(at-line 33 '(exactly 1)))
        loc (-> form zip/seq-zip zip/down)]
    loc => loc-is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "If both sides have line numbers, the left takes precedence: (f 1) => (exactly 1)"
  (let [form `( ~(at-line 33 '(f 1)) => ~(at-line 34 '(exactly 1)))
        loc (-> form zip/seq-zip zip/down)]
    loc => loc-is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "If neither side has a line number, look to the left and add 1: (let [a 2] a => b)"
  (let [form `( (let ~(at-line 32 '[a 2]) a => b))
        loc (-> form zip/seq-zip zip/down zip/down zip/right zip/right)]
    loc => loc-is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => 33)

  "Default retult is nil."
  (let [form '(1 => 2)
        loc (-> form zip/seq-zip zip/down)]
    loc => loc-is-start-of-arrow-sequence?
    (arrow-line-number (zip/right loc)) => nil))

