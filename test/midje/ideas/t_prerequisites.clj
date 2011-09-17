;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-prerequisites
  (:use midje.ideas.prerequisites
        [midje.internal-ideas.expect :only [expect?]]
        [midje.ideas.metaconstants :only [metaconstant-for-form]]
        midje.sweet midje.test-util)
  (:require [clojure.zip :as zip]))

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))]
    (-> values zip/down) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right zip/right) =not=> is-head-of-form-providing-prerequisites?))

(fact "can convert prerequisites into fake calls"
  (let [original '( provided                        (f 1) => 3                         (f 2) => (+ 1 1))
        translated '(        (midje.semi-sweet/fake (f 1) => 3) (midje.semi-sweet/fake (f 2) => (+ 1 1)))
        z (zip/seq-zip original)
        loc (zip/down z)]
    (expand-prerequisites-into-fake-calls loc) => translated)
  "including metaconstant prerequisites"
  (let [original '( provided                             ...m... =contains=> {:a 'a})
        translated '(        (midje.semi-sweet/data-fake ...m... =contains=> {:a 'a}) )
        z (zip/seq-zip original)
        loc (zip/down z)]
    (expand-prerequisites-into-fake-calls loc) => translated))
  

(fact "created fakes have the line number of the arrow form"
  (let [args `( ~(at-line 789 '(f 1)) => 3)]
    (:line (meta (prerequisite-to-fake args))) => 789))
     
(fact "prerequisite containers are deleted so their contents can be inserted elsewhere"
  (let [original '( (expect (f x) => (+ 1 2)) (provided ...) "next")
        edited   '( (expect (f x) => (+ 1 2))                "next")
        z (zip/seq-zip original)
        original-loc (-> z zip/down zip/right zip/down)
        resulting-loc
         (delete_prerequisite_form__then__at-previous-full-expect-form original-loc)]
        
    original-loc => is-head-of-form-providing-prerequisites?
    resulting-loc => expect?
    (zip/root resulting-loc) => edited))
    



