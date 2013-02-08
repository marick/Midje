(ns midje.parsing.1-to-explicit-form.t-prerequisites
  (:use midje.parsing.1-to-explicit-form.prerequisites
        midje.sweet midje.test-util)
  (:require [clojure.zip :as zip]
            [midje.parsing.util.recognizing :as recognize]))

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
        
    original-loc => recognize/provided?
    resulting-loc => recognize/expect?
    (zip/root resulting-loc) => edited))

(facts "the run-on string of arrow forms can be grouped into a list of arrow sequences"
  ;; Use of let is to prevent #'fact from slapping a line number onto the results.
  (let [result (pull-all-arrow-seqs-from '(   (f 1) => 2    (g 1) => 3))]
    result =>                         '(  [(f 1) => 2]  [(g 1) => 3] ))

  (let [result (pull-all-arrow-seqs-from '(  (f 1) => 2 :key value   (g 1) => 3))]
    result =>                         '( [(f 1) => 2 :key value] [(g 1) => 3])))
