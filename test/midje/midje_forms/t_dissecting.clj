;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-dissecting
  (:require [clojure.zip :as zip])
  (:use [midje.midje-forms dissecting recognizing]
        [midje.error-handling monadic sweet-errors]
        [midje sweet test-util])
  (:use [ordered.map :only (ordered-map)]))

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
    (arrow-form-overrides '( :expected-result 3 :position "foo.clj:33"))
    => '(:expected-result 3 :position "foo.clj:33")

    "Does not scoop up following forms"
    (arrow-form-overrides '( :expected-result 3 :position "foo.clj:33" (f 1)))
    => '(:expected-result 3 :position "foo.clj:33")

    "... even if those following forms have their own overrides"
    (arrow-form-overrides '( :expected-result 3 :position "foo.clj:33"
                                   (f 1) => 1 :expected-result 2))
    => '(:expected-result 3 :position "foo.clj:33"))

(facts "the run-on string of arrow forms can be partitioned"
  ;; Use of let is to prevent #'fact from slapping a line number onto the results.
  (let [result (partition-arrow-forms '(   (f 1) => 2    (g 1) => 3))]
    result =>                         '(  [(f 1) => 2]  [(g 1) => 3] ))

  (let [result (partition-arrow-forms '(  (f 1) => 2 :key value   (g 1) => 3))]
    result =>                         '( [(f 1) => 2 :key value] [(g 1) => 3])))

;; Fact tables
 
(fact "gets the bindings off fact table"
  (table-binding-maps (list '?a  '?b '?result
                              1   2   3))
    => [ (ordered-map '?a 1, '?b 2, '?result 3) ])
 
(fact "ignores pipes"
  (table-binding-maps (list '?a '| '?b '| '?result
                              1 '|  2  '|  3))
    => [ (ordered-map '?a 1, '?b 2, '?result 3) ])
 
(fact "ignores symbol - where"
  (table-binding-maps (list 'where
  		            '?a '?b '?result
                              1   2   3))
    => [ (ordered-map '?a 1, '?b 2, '?result 3) ])
 
(fact "ignores keyword - :where"
  (table-binding-maps (list :where
  		            '?a '?b '?result
                              1   2   3))
    => [ (ordered-map '?a 1, '?b 2, '?result 3) ])
 
(fact "no trouble with :where or where in the data"
  (table-binding-maps (list :where
  		            '?a      '?b      '?result
                             'where   :where   "where:where"))
    => [ (ordered-map '?a 'where, '?b :where, '?result "where:where") ])

(after-silently
 (tabular
  (fact "error message"
    (tabular-forms '?forms) => '?expected
    ?forms                       ?expect
    [ fact table ]               [fact table]))
 (fact (one-of user-error)))

(tabular "can split apart fact forms with optional doc-string"
 (fact 
   (let [s "string"]
     (validate '?forms) => '?expected))
   ?forms                               ?expected
   (tabular fact table...)              [fact [table...]]
   (tabular "string" fact table...)     [fact [table...]]
   ;; Doesn't work with non-literal strings
   (tabular s fact table...)            [s [fact table...]])

