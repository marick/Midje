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

(defn check-dissects-fact-table [msg [fact-form & table]]
  (let [dissected (dissect-fact-table table)
        expected-fact-form '(fact (+ ?a ?b) => ?result)
        expected-binding-maps '[ {?a 1, ?b 2, ?result 3} ]
        expected-variable-order '[?a ?b ?result]]
  
    (facts
      (:fact-form dissected) => expected-fact-form
      (:binding-maps dissected) => expected-binding-maps
      (:variable-order dissected) => expected-variable-order)))

(check-dissects-fact-table "basic table"
  '(tabular
     (fact (+ ?a ?b) => ?result)
       ?a     ?b      ?result
        1      2       3))

(check-dissects-fact-table "ignores pipes"
  '(tabular
     (fact (+ ?a ?b) => ?result)
       ?a | ?b | ?result
       1  | 2  | 3))

(check-dissects-fact-table "ignores symbol - where" 	
  '(tabular    
    (fact (+ ?a ?b) => ?result)
    
      where	 	
       ?a  ?b  ?result	 	
       1   2   3))	 	
	 	
(check-dissects-fact-table "ignores keyword - :where"	 	
  '(tabular	 	
     (fact (+ ?a ?b) => ?result)	
 	
      :where	 	
      ?a  ?b  ?result	 	
      1   2   3))

(let [dissected (dissect-fact-table (rest '(tabular
                                         (fact (str ?a ?b) => ?result)
                                       ?a     | ?b     | ?result
                                       'where | :where | "where:where")))
      expected-fact-form '(fact (str ?a ?b) => ?result)
      expected-binding-maps '[ { ?a (quote where), ?b :where, ?result "where:where"} ]
      expected-variable-order '[?a ?b ?result]]
  
  (facts "has no trouble with :where or where in the data"
    (:fact-form dissected) => expected-fact-form
    (:binding-maps dissected) => expected-binding-maps
    (:variable-order dissected) => expected-variable-order))
