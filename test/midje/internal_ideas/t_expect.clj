;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.t-expect
  (:use [midje.internal-ideas.expect]
        [midje.ideas.arrows :only [is-start-of-arrow-sequence?]]
	midje.sweet
	midje.test-util)
  (:require [clojure.zip :as zip])
)

(fact "can position so loc is the entire expect form"
  (let [z (zip/seq-zip '(expect (f 1) => (+ 1 1)))
	finds-enclosing (fn [loc] (= (zip/node (zip/down loc)) 'expect))]
    (up-to-full-expect-form z) => finds-enclosing
    (up-to-full-expect-form (zip/down z)) => finds-enclosing
    (up-to-full-expect-form (-> z zip/down zip/rightmost)) => finds-enclosing
    (up-to-full-expect-form (-> z zip/down zip/rightmost zip/down)) => finds-enclosing))

(tabular 
 (fact "an embedded expect form can be recognized"
   (expect? (zip/seq-zip ?form)) => ?expected)

 ?form                                  ?expected
 '(expect x => y)                       truthy
 '(midje.semi-sweet/expect x => y)      truthy
 '(+ x y)                               falsey
 'expect                                falsey)


(fact "can append forms to end of top-level of expect form"
  (let [original '( (expect ...                                ) "next")
        edited   '( (expect ... (fake (f) => 2) (fake (g) => 3)) "next")
        z            (zip/seq-zip original)
        original-loc (-> z zip/down)
        resulting-loc
           (tack-on__then__at-same-location '((fake (f) => 2) (fake (g) => 3)) original-loc)]
    original-loc => expect?
    resulting-loc => expect?
    (zip/root resulting-loc) => edited))

(defn node [expected] (fn [actual] (= expected (zip/node actual))))

(fact "sweet-style facts can be converted to semi-sweet expect forms"
  "The simple case"
  (let [original '(                          (f 1) => (+ 2 3)  "next")
        edited   '( (midje.semi-sweet/expect (f 1) => (+ 2 3)) "next")
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    original-loc => (node '(f 1))
    original-loc => is-start-of-arrow-sequence?
    
    (zip/root resulting-loc) => edited
    (zip/next resulting-loc) => (node "next"))


  "A negating check"
  (let [original '(                          (f 1) =not=> (+ 2 3)  "next")
        edited   '( (midje.semi-sweet/expect (f 1) =not=> (+ 2 3)) "next")
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    original-loc => (node '(f 1))
    original-loc => is-start-of-arrow-sequence?
    
    (zip/root resulting-loc) => edited
    (zip/next resulting-loc) => (node "next"))


  (let [original '(                          (f 1) => (+ 2 3) :key "value"  "next")
        edited   '( (midje.semi-sweet/expect (f 1) => (+ 2 3) :key "value") "next")   
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    original-loc => (node '(f 1))
    original-loc => is-start-of-arrow-sequence?
    
   (zip/root resulting-loc) => edited
   (zip/next resulting-loc) => (node "next"))

  "annotations on the original form are preserved"
  (let [original '(                          (f 1) => (+ 2 3) :key "value")
        edited   '( (midje.semi-sweet/expect (f 1) => (+ 2 3) :key "value"))   
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    (zip/root resulting-loc) => edited
    (zip/next resulting-loc) => zip/end?)

  "The new expect form has the same line number as the arrow"
  (let [original `( ~(at-line 505 '(f 1)) => 1)
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    (:line (meta (first (zip/root resulting-loc)))) => 505))

  


