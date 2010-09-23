(ns midje.sweet.t-chained-fakes-test
  (:use [midje.sweet.chained-fakes] :reload-all)
  (:use midje.semi-sweet)
  (:use [clojure.test])
  (:require [clojure.zip :as zip])
  (:use [midje.test-util]))

;; (fake (f (g 1)) => 3)
;; (fake (f (g 1)) => 3 :line-number kdslfkj)
;; (fake (f (quote ()))
;; (fake (f (macro 33)))


(defmacro some-macro [& rest] )
(deftest discovering-chained-fake-examples
  (let [run (fn [form] (at-chained-fake? (zip/seq-zip form)))]
    (expect (run '()) => falsey)
    (expect (run '(+ 1 2)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f 1) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f '(l)) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f [l]) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f {a 1}) => 3)) => falsey)
    ;; This next is surprisingly hard to get right.
    ;; Note also that (currently) symbols-in-the-function-slot don't
    ;; have to predefined to be faked, which is another case to worry about.
;    (expect (run '(midje.semi-sweet/fake (f (some-macro 33)) => 3)) => falsey)
    (expect (run '(midje.semi-sweet/fake (f (g 3)) => 33)) => truthy)
    ;; Sad but true: a cons is not a list.
    (expect (run (cons 'midje.semi-sweet/fake '((f (g 3)) => 33))) => truthy)
))



(deftest first-form-metaconstant-for-a-function-symbol-example
  (expect (form-metaconstant '(g)) => '...g-link...))
     
(deftest pull-out-simple-chained-function-example
  (expect (form-to-pull-out '(fake (f (g)) => 3)) => '(g)))
	    
(deftest replace-with-metaconstant-example
  (expect (replace-interior-function-with-metaconstant
	    '(fake (f   (g))   => 3)          '(g) '...g...) =>
	    '(fake (f ...g...) => 3))
  (expect (replace-interior-function-with-metaconstant
	    '(fake (f   (g))   => 3 :key 'val)    '(g) '...g...) =>
	    '(fake (f ...g...) => 3 :key 'val)))

(deftest trivial-example-of-unchaining-a-fake
  (let [input-form '(fake (f (g)) => 3)]
    (expect (unchain input-form) =>
	      [ '(midje.semi-sweet/fake (g) midje.semi-sweet/=> ...g-value...)
		'(fake (f ...g-value...) => 3) ]
	    (fake (form-to-pull-out input-form) => '(g))
	    (fake (form-metaconstant '(g)) => '...g-value...)
	    (fake (replace-interior-function-with-metaconstant
		    '(fake (f (g)) => 3) '(g) '...g-value...) => '(fake (f ...g-value...) => 3)))))

(deftest when-unchaining-a-fake-keyword-arguments-are-pulled-into-both-result-fakes
  (let [input-form '(fake (f (g)) => 3 :key 'value)]
    (expect (unchain input-form) =>
	      [ '(midje.semi-sweet/fake (g) midje.semi-sweet/=> ...g-value... :key 'value)
		'(fake (f ...g-value...) => 3 :key 'value) ]
	      (fake (form-metaconstant '(g)) => '...g-value...))))
	      
(declare ...first-fake-link... ...second-fake-link...)
(deftest replacing-a-fake-with-two-fakes-example
  (let [input-form '(x (fake (f (g 1)) => 3 :key 'value) y)
        loc (-> input-form zip/seq-zip zip/down zip/right)]
    (assert (at-chained-fake? loc))
    ;; result
    (expect (zip/root (replace-with-two-links__stay_put loc)) =>
	    '(x (fake ...first-fake-link...) (fake ...second-fake-link...) y)
	    (fake (unchain '(fake (f (g 1)) => 3 :key 'value)) =>
		  '[(fake ...first-fake-link...) (fake ...second-fake-link...)]))
    ;; Location
    (expect (zip/node (replace-with-two-links__stay_put loc)) =>
	    '(fake ...first-fake-link...)
	    (fake (unchain '(fake (f (g 1)) => 3 :key 'value)) =>
		  '[(fake ...first-fake-link...) (fake ...second-fake-link...)]))))
