;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-facts
  (:use midje.ideas.facts
        midje.sweet midje.test-util)
  (:require [clojure.zip :as zip]))

;; Translating sweet forms into their semi-sweet equivalent


(fact "translating entire fact forms"
  "some parts of a fact are to be left alone"
  (let [form '(a-form-would-go-here another-would-go-here)]
    (to-semi-sweet form) => form)

  (let [form '( (nested (form) form ) [ 1 2 3])]
    (to-semi-sweet form) => form)

  "arrow sequences are wrapped with expect"
  (let [form '(                              (f 1)                  => [2]                           (f 2)                  => (+ 1 2) )
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

