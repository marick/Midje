(ns midje.ideas.t-facts
  (:use midje.ideas.facts
        midje.sweet midje.test-util)
  (:require [clojure.zip :as zip]))

;; Fact metadata

(def a-body '((f) => 3))

(fact "contains the original source"
    (let [[meta _] (separate-fact-metadata `(fact "doc" ~@a-body))]
      (:midje/source meta) => `(fact "doc" ~@a-body)))


(fact "doc strings"
  (fact "can be separated"
    (let [[meta body] (separate-fact-metadata `(fact "doc" ~@a-body))]
      (:midje/description meta) => "doc"
      body => a-body))
  (fact "need not be present"
    (let [[meta body] (separate-fact-metadata `(fact ~@a-body))]
      (:midje/description meta) => nil
      body => a-body))
  (fact "can provide the name"
    (let [[meta body] (separate-fact-metadata `(fact "doc" ~@a-body))]
      (:midje/name meta) => "doc"
      body => a-body))
  )

  
(facts "symbols"
  (fact "become the fact name"
    (let [[meta body] (separate-fact-metadata `(fact cons ~@a-body))]
      (:midje/name meta) => "cons"
      body => a-body))
  (fact "take precedence over strings"
    (let [[meta body] (separate-fact-metadata `(fact "foo" cons ~@a-body))]
      (:midje/name meta) => "cons"
      body => a-body)
    (let [[meta body] (separate-fact-metadata `(fact cons "foo" ~@a-body))]
      (:midje/name meta) => "cons"
      body => a-body))
  (fact "don't count as names when they are the head of an expect form"
    (let [[meta body] (separate-fact-metadata `(fact foo => 3))]
      (:midje/name meta) => nil
      body => `(foo => 3)))
    )




;; Translating sweet forms into their semi-sweet equivalent


(fact "translating entire fact forms"
  "some parts of a fact are to be left alone"
  (let [form '(a-form-would-go-here another-would-go-here)]
    (to-semi-sweet form) => form)

  (let [form '( (nested (form) form ) [ 1 2 3])]
    (to-semi-sweet form) => form)

  "arrow sequences are wrapped with expect"
  (let [form '(                              (f 1) => [2]                           (f 2) => (+ 1 2) )
        expected '( (midje.semi-sweet/expect (f 1) => [2]) (midje.semi-sweet/expect (f 2) => (+ 1 2)))]
    (expect (to-semi-sweet form) => expected))

  "the wrapping can include prerequisites turned into fake forms."
  (let [form '( (f 1) => [1] :ekey "evalue"
                (f 2) => (+ 2 2)
                (provided (g 3) => 3
                          (g 4) => 4 :pkey "pvalue")
                (f 5) => truthy)
        expected '( (midje.semi-sweet/expect (f 1) => [1] :ekey "evalue")
                    (midje.semi-sweet/expect (f 2) => (+ 2 2)
                                             (midje.semi-sweet/fake (g 3) => 3)
                                             (midje.semi-sweet/fake (g 4) => 4 :pkey "pvalue"))
                    (midje.semi-sweet/expect (f 5) => truthy))]
    (to-semi-sweet form) => expected)

  "It's useful to embed expect clauses with notcalled prerequisites, so they're skipped"
  (let [form '(    (expect (f 1) => 2 (fake (g 1) => 2))
                                      (fake (m 1) => 33))]
    (to-semi-sweet form) => form))


(each-causes-validation-error #"There is no arrow in your fact form"
  (fact)
  (fact "vector fact" [1 2 3 4] (contains 3)))

(causes-validation-error #"There is no arrow in your facts form" 
  (facts 1))
