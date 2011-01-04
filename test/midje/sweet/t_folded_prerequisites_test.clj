(ns midje.sweet.t-folded-prerequisites-test
  (:use [midje.sweet.folded-prerequisites])
  (:use midje.semi-sweet)
  (:use [clojure.test])
  (:require [clojure.zip :as zip])
  (:use [midje.test-util]))

(defmacro some-macro [& rest] )
(deftest discovering-folded-prerequisite-examples
  (let [run (fn [form] (at-folded-prerequisite? (zip/seq-zip form)))]
    (expect (run '()) => falsey)
    (expect (run '(+ 1 2)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f 1) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f '(l)) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f [l]) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f {a 1}) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake
		   (f (midje.util.checkers/in-any-order [1 2 3])) => 33)) => falsey)
    ;; This next is surprisingly hard to get right.
    ;; Note also that (currently) symbols-in-the-function-slot don't
    ;; have to predefined to be faked, which is another case to worry about.
;    (expect (run '(midje.semi-sweet/fake (f (some-macro 33)) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f (g 3)) => 33)) => truthy)
    ;; Sad but true: a cons is not a list.
    (expect (run (cons 'midje.semi-sweet/fake '((f (g 3)) => 33))) => truthy)
))



(deftest metaconstants-for-a-function-symbol-example
  (with-count-atom
    (expect (form-metaconstant '(g)) => '...g-value-1...) ; Revenge of the English major!
    (expect (form-metaconstant '(g)) => '...g-value-2...)
    (expect (form-metaconstant '(h)) => '...h-value-1...)))

     
(deftest pull-out-simple-folded-prerequisite-example
  (expect (form-to-pull-out '(fake (f (g)) => 3)) => '(g)))
	    
(deftest replace-with-metaconstant-example
  (expect (replace-interior-function-with-metaconstant
	    '(fake (f   (g))   => 3)          '(g) '...g...) =>
	    '(fake (f ...g...) => 3))
  (expect (replace-interior-function-with-metaconstant
	    '(fake (f   (g))   => 3 :key 'val)    '(g) '...g...) =>
	    '(fake (f ...g...) => 3 :key 'val)))

(deftest trivial-example-of-unfolding-a-prerequisite
  (let [input-form '(fake (f (g)) => 3)]
    (expect (unfold input-form) =>
	      [ '(midje.semi-sweet/fake (g) midje.semi-sweet/=> ...g-value...)
		'(fake (f ...g-value...) => 3) ]
	    (fake (form-to-pull-out input-form) => '(g))
	    (fake (form-metaconstant '(g)) => '...g-value...)
	    (fake (replace-interior-function-with-metaconstant
		    '(fake (f (g)) => 3) '(g) '...g-value...) => '(fake (f ...g-value...) => 3)))))

(deftest when-unfolding-a-prerequisite-keyword-arguments-are-pulled-into-both-resulting-prerequisites
  (let [input-form '(fake (f (g)) => 3 :key 'value)]
    (expect (unfold input-form) =>
	      [ '(midje.semi-sweet/fake (g) midje.semi-sweet/=> ...g-value... :key 'value)
		'(fake (f ...g-value...) => 3 :key 'value) ]
	      (fake (form-metaconstant '(g)) => '...g-value...))))
	      
(declare ...first-unfolded... ...second-unfolded...)
(deftest replacing-a-prerequisite-with-the-unfolded-version
  (let [input-form '(x (fake (f (g 1)) => 3 :key 'value) y)
        loc (-> input-form zip/seq-zip zip/down zip/right)]
    (assert (at-folded-prerequisite? loc))
    ;; result
    (expect (zip/root (replace-with-two-prerequisites__stay_put loc)) =>
	    '(x (fake ...first-unfolded...) (fake ...second-unfolded...) y)
	    (fake (unfold '(fake (f (g 1)) => 3 :key 'value)) =>
		  '[(fake ...first-unfolded...) (fake ...second-unfolded...)]))
    ;; Location
    (expect (zip/node (replace-with-two-prerequisites__stay_put loc)) =>
	    '(fake ...first-unfolded...)
	    (fake (unfold '(fake (f (g 1)) => 3 :key 'value)) =>
		  '[(fake ...first-unfolded...) (fake ...second-unfolded...)]))))
