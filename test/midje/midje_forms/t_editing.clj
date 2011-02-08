;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-editing
  (:use [midje.midje-forms.editing]
        [midje.midje-forms.recognizing]
        clojure.test
        midje.sweet
        midje.test-util)
  (:require [clojure.zip :as zip])
)

(defn node [expected] (fn [actual] (= expected (zip/node actual))))

(fact "it's useful to delete a node and move right"
  (let [z (zip/seq-zip '( (f n) => (+ 3 4)))
        loc (-> z zip/down zip/right)]
    (remove-moving-right loc) => (node '(+ 3 4))
    (zip/root (remove-moving-right loc)) => '( (f n) (+ 3 4))))


(fact "prerequisite containers are deleted so their contents can be inserted elsewhere"
  (let [original '( (expect (f x) => (+ 1 2)) (provided ...) "next")
        edited   '( (expect (f x) => (+ 1 2))                "next")
        z (zip/seq-zip original)
        original-loc (-> z zip/down zip/right zip/down)
        resulting-loc
         (delete_prerequisite_form__then__at-previous-full-expect-form original-loc)]
        
    original-loc => loc-is-head-of-form-providing-prerequisites?
    resulting-loc => loc-is-at-full-expect-form?
    (zip/root resulting-loc) => edited))
    
(fact "can append forms to end of top-level of expect form"
  (let [original '( (expect ...                                ) "next")
        edited   '( (expect ... (fake (f) => 2) (fake (g) => 3)) "next")
        z            (zip/seq-zip original)
        original-loc (-> z zip/down)
        resulting-loc
           (tack-on__then__at-same-location '((fake (f) => 2) (fake (g) => 3)) original-loc)]
    original-loc => loc-is-at-full-expect-form?
    resulting-loc => loc-is-at-full-expect-form?
    (zip/root resulting-loc) => edited))

(fact "sweet-style facts can be converted to semi-sweet expect forms"
  "The simple case"
  (let [original '(                          (f 1)                  => (+ 2 3)  "next")
        edited   '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> (+ 2 3)) "next")
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    original-loc => (node '(f 1))
    original-loc => loc-is-start-of-arrow-sequence?
    
   (zip/root resulting-loc) => edited
   (zip/next resulting-loc) => (node "next"))


  (let [original '(                          (f 1)                  => (+ 2 3) :key "value"  "next")
        edited   '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> (+ 2 3) :key "value") "next")   
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    original-loc => (node '(f 1))
    original-loc => loc-is-start-of-arrow-sequence?
    
   (zip/root resulting-loc) => edited
   (zip/next resulting-loc) => (node "next"))

  "annotations on the original form are preserved"
  (let [original '(                          (f 1)                  => (+ 2 3) :key "value")
        edited   '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> (+ 2 3) :key "value"))   
        z             (zip/seq-zip original)
        original-loc  (-> z zip/down)
        resulting-loc (wrap-with-expect__then__at-rightmost-expect-leaf original-loc)]
    (zip/root resulting-loc) => edited
    (zip/next resulting-loc) => zip/end?))


(fact "one can add a line number to an arrow sequence"
  (let [original '( (f n) => 2  )
        expected '( (f n) => 2 :position (midje.util.file-position/line-number-known 10))
        z            (zip/seq-zip original)
        original-loc (-> z zip/down zip/right)
        new-loc      (add-line-number-to-end-of-arrow-sequence__then__no-movement
                        10 original-loc)]
    (name (zip/node new-loc)) => "=>"
    (zip/root new-loc) => expected))

(fact "one fake can be replaced with two"
  (let [original '(expect (f 1) => 2 (fake (f (g)) => 3))
        loc (-> original zip/seq-zip zip/down zip/right zip/right zip/right zip/right)
        new-loc (replace-one-fake-with-two__then__stay_put
                  loc '[(fake FAKE1) (fake FAKE2)])
        expected '(expect (f 1) => 2 (fake FAKE1) (fake FAKE2))]
    (zip/root new-loc) => expected
    (zip/node new-loc) => '(fake FAKE1)))
  
