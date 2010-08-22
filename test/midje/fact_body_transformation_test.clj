(ns midje.fact-body-transformation-test
  (:use [midje.fact-body-transformation] :reload-all)
  (:use midje.semi-sweet)
  (:use clojure.test)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(defn node [expected] (fn [actual] (= expected (zip/node actual))))
  

(deftest ignoring-certain-forms-test
  (doseq [skippable '(expect fake)]
    (let [z (zip/seq-zip (list 111 (list skippable 1 2 3) :next))
	  skippable (-> z zip/down zip/next zip/down)]
      (expect (ignore-form-headed-by? skippable) => truthy)
      (expect (zip/next (skip skippable)) => (node :next))
      )))

(deftest deleting-provided-forms-test
  (let [z (zip/seq-zip '( (expect ...) (provided ...) :next))
	loc (-> z zip/down zip/right zip/down)]
    (expect (fake-source? loc) => truthy)
    (let [resulting-loc (delete-fake-source loc)
	  leaves-location-on-previous-form? #(= (zip/node %) '(expect ...))]
      (expect (zip/root resulting-loc) => '((expect ...) :next))
      (expect (leaves-location-on-previous-form? resulting-loc) => truthy))))

(deftest convert-provided-body-into-fake-calls-test
  (let [form '( provided (f 1) => 3 (f 2) => (+ 1 1))]
    (expect (expand-into-fake-calls form) => '( (midje.semi-sweet/fake (f 1) => 3)
						(midje.semi-sweet/fake (f 2) => (+ 1 1))))))

(deftest appending-to-current-form-test
  (let [z (zip/seq-zip '( (expect ...) :next))
	loc (-> z zip/down)]
    (expect (zip/node loc) => '(expect ...))
    (let [resulting-loc (tack-on '(fake (f) => 2) loc)]
      (expect (zip/next resulting-loc) => (node :next))
      (expect (zip/root resulting-loc) => '( (expect ... (fake (f) => 2)) :next)))))

(deftest detecting-need-for-expect-test
  (let [z (zip/seq-zip '( (f 1) ))
	loc (-> z zip/down)]
    (expect (needs-wrapping-with-expect? loc) => falsey))

    (let [z (zip/seq-zip '( (f 1) (f 2)))
	loc (-> z zip/down)]
    (expect (needs-wrapping-with-expect? loc) => falsey))

    (let [z (zip/seq-zip '( (f 1) => 2))
	loc (-> z zip/down)]
    (expect (needs-wrapping-with-expect? loc) => truthy)))


(deftest wrapping-with-expect-test
  (let [z (zip/seq-zip '( (f 1) => 2 :next))
	loc (-> z zip/down)]
    (expect (zip/node loc) => '(f 1))
    (expect (needs-wrapping-with-expect? loc) => truthy)
    (let [resulting-loc (wrap-with-expect loc)]
      (expect (zip/next resulting-loc) => (node :next))
      (expect (zip/root resulting-loc) => '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> 2) :next)))))

;; top-level

(deftest rewrite-trivial-form-test
  (let [form '(a-form-would-go-here another-would-go-here)]
    (expect (rewrite form) => form))

  (let [form '( (nested (form) form ) [ 1 2 3])]
    (expect (rewrite form) => form)))


(deftest ignores-semi-sweet-constructs-test
  (let [form '(    (expect (f 1) => 2 (fake (g 1) => 2))
		   (fake (m 1) => 33))]
    (expect (rewrite form) => form)))

(deftest wraps-forms-in-expect
  (let [form '( (f 1) => [2]
		(f 2) => (+ 1 2) )
	expected '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> [2])
		    (midje.semi-sweet/expect (f 2) midje.semi-sweet/=> (+ 1 2)))]
    (expect (rewrite form) => expected)))


  
