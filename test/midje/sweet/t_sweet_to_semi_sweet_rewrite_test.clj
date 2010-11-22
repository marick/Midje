(ns midje.sweet.t-sweet-to-semi-sweet-rewrite-test
  (:use [midje.sweet.sweet-to-semi-sweet-rewrite] :reload-all)
  (:use midje.semi-sweet)
  (:use midje.util.recognizing-forms)
  (:use clojure.test)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(defn node [expected] (fn [actual] (= expected (zip/node actual))))

;; Identifying forms

(deftest should-ignore-expect-and-fake
  (doseq [skippable '(expect fake midje.semi-sweet/expect midje.semi-sweet/fake)]
    (let [z (zip/seq-zip (list 111 (list skippable 1 2 '(3)) "next"))
	  skippable (-> z zip/down zip/next zip/down)]
      (expect (is-semi-sweet-keyword? skippable) => truthy)
      (expect (zip/next (skip-to-end-of-full-form skippable)) => (node "next"))
      )))

(deftest should-know-when-at-full-expect-form
  (expect (at-full-expect-form? (zip/seq-zip '(expect x => y))) => truthy)
  (expect (at-full-expect-form? (zip/seq-zip '(midje.semi-sweet/expect x => y))) => truthy)
  (expect (at-full-expect-form? (zip/seq-zip '(+ x y))) => falsey)
  (expect (at-full-expect-form? (zip/seq-zip 'expect)) => falsey))


(deftest should-know-head-of-provided-form
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))
	simple (zip/down values)
	qualified (zip/right simple)
	incorrect (zip/right qualified)]
    (expect (head-of-provided-form? simple) => truthy)
    (expect (head-of-provided-form? qualified) => truthy)
    (expect (head-of-provided-form? incorrect) => falsey)))

(deftest should-know-when-at-sequence-that-needs-rewriting-into-semi-sweet-style
  (let [z (zip/seq-zip '( (f 1) ))
	loc (-> z zip/down)]
    (expect (start-of-arrow-sequence? loc) => falsey))

    (let [z (zip/seq-zip '( (f 1) (f 2)))
	loc (-> z zip/down)]
    (expect (start-of-arrow-sequence? loc) => falsey))

    (let [z (zip/seq-zip '( (f 1) => 2))
	loc (-> z zip/down)]
      (expect (start-of-arrow-sequence? loc) => truthy))
    
    (let [z (zip/seq-zip '( (f 1) midje.semi-sweet/=> 2))
	loc (-> z zip/down)]
      (expect (start-of-arrow-sequence? loc) => truthy)))

;; Munging ordinary forms

(deftest should-produce-list-of-overrides
  (let [move-to-loc (fn [root] (-> root zip/down zip/right zip/right))
	start (fn [form] (move-to-loc (zip/seq-zip form)))]

    (expect (overrides '()) => '())
    (expect (overrides '((g 1) => 1)) => '())
    (let [form '( :expected-result 3 :file-position "foo.clj:33")]
      (expect (overrides form) => '(:expected-result 3 :file-position "foo.clj:33")))
    (let [form '( :expected-result 3 :file-position "foo.clj:33" (f 1))]
      (expect (overrides form) => '(:expected-result 3 :file-position "foo.clj:33")))
    (let [form '( :expected-result 3 :file-position "foo.clj:33"
		  (f 1) => 1 :expected-result 2)]
      (expect (overrides form) => '(:expected-result 3 :file-position "foo.clj:33")))
))


(deftest should-partition-fake-bodies
  (expect (partition-fake-bodies '(  (f 1) => 2   (g 1) => 3)) =>
   	                         '( [(f 1) => 2] [(g 1) => 3]))

  (expect (partition-fake-bodies '(  (f 1) => 2 :key value   (g 1) => 3)) =>
	                         '( [(f 1) => 2 :key value] [(g 1) => 3]))
)




;; Simple movement

(deftest should-be-able-to-position-at-enclosing-full-expect-form
  (let [z (zip/seq-zip '(expect (f 1) => (+ 1 1)))
	finds-enclosing (fn [loc] (= (zip/node (zip/down loc)) 'expect))]
    (expect (up-to-full-expect-form z) => finds-enclosing)
    (expect (up-to-full-expect-form (zip/down z)) => finds-enclosing)
    (expect (up-to-full-expect-form (-> z zip/down zip/rightmost)) => finds-enclosing)
    (expect (up-to-full-expect-form (-> z zip/down zip/rightmost zip/down)) => finds-enclosing)))


;; Editing

(deftest should-be-able-to-delete-and-end-up-at-next-form-of-same-level
  (let [z (zip/seq-zip '( (f n) => (+ 3 4)))
	loc (-> z zip/down zip/right)]
    (expect (zip/node (remove-moving-right loc)) => '(+ 3 4))
    (expect (zip/root (remove-moving-right loc)) => '( (f n) (+ 3 4)))))
  


(deftest should-be-able-to-delete-provided-form-and-position-for-appending
  (let [z (zip/seq-zip '( (expect (f x) => (+ 1 2)) (provided ...) "next"))
	loc (-> z zip/down zip/right zip/down)]
    (expect (head-of-provided-form? loc) => truthy)
    (let [resulting-loc (delete-enclosing-provided-form__at-previous-full-expect-form loc)]
      (expect (zip/root resulting-loc) => '((expect (f x) => (+ 1 2)) "next"))
      (expect resulting-loc => at-full-expect-form?))))

(deftest should-be-able-to-convert-provided-body-into-fake-calls
  (let [z (zip/seq-zip '( provided (f 1) => 3 (f 2) => (+ 1 1)))
	loc (zip/down z)]
    (expect (expand-following-into-fake-calls loc) => '( (midje.semi-sweet/fake (f 1) => 3)
							  (midje.semi-sweet/fake (f 2) => (+ 1 1))))))

(deftest should-be-able-to-append-to-expect-form
  (let [z (zip/seq-zip '( (expect ...) "next"))
	loc (-> z zip/down)]
    (expect loc => at-full-expect-form?)
    (let [resulting-loc (tack-on__at-same-location '((fake (f) => 2) (fake (g) => 3)) loc)]
      (expect resulting-loc => at-full-expect-form?)
      (expect (zip/root resulting-loc) => '( (expect ...
						     (fake (f) => 2)
						     (fake (g) => 3))
					     "next")))))

(deftest should-be-able-to-wrap-expect-forms
  (let [z (zip/seq-zip '( (f 1) => (+ 2 3) "next"))
	loc (-> z zip/down)]
    (expect (zip/node loc) => '(f 1))
    (expect (start-of-arrow-sequence? loc) => truthy)
    (let [resulting-loc (wrap-with-expect__at-rightmost-wrapped-location loc)]
      (expect (zip/next resulting-loc) => (node "next"))
      (expect (zip/root resulting-loc) => '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> (+ 2 3)) "next")))))

(deftest should-be-able-to-include-annotations-in-expect-form-wrapping
  (let [z (zip/seq-zip '( (f 1) => (+ 2 3) :key "value" "next"))
	loc (-> z zip/down)]
    (expect (zip/node loc) => '(f 1))
    (expect (start-of-arrow-sequence? loc) => truthy)
    (let [resulting-loc (wrap-with-expect__at-rightmost-wrapped-location loc)]
      (expect (zip/next resulting-loc) => (node "next"))
      (expect (zip/root resulting-loc) =>
	      '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> (+ 2 3) :key "value") "next")))))

(deftest expectations-at-end-of-form-are-successfully-wrapped
  (let [z (zip/seq-zip '( (f 1) => (+ 2 3) :key "value"))
	loc (-> z zip/down)]
    (expect (zip/node loc) => '(f 1))
    (expect (start-of-arrow-sequence? loc) => truthy)
    (let [resulting-loc (wrap-with-expect__at-rightmost-wrapped-location loc)]
      (expect (zip/next resulting-loc) => zip/end?)
      (expect (zip/root resulting-loc) =>
	      '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> (+ 2 3) :key "value"))))))

;; ;; top-level

(deftest rewrite-trivial-form-test
  (let [form '(a-form-would-go-here another-would-go-here)]
    (expect (rewrite form) => form))

  (let [form '( (nested (form) form ) [ 1 2 3])]
    (expect (rewrite form) => form)))

  ;; At some point, it may be useful not to painstakingly skip over
  ;; generated or edited (expect) or (fake) trees. In that case,
  ;; reintroduce this test and make it pass by removing a comment
  ;; from (rewrite).
;; (deftest ignores-semi-sweet-constructs-test
;;   (let [form '(    (expect (f 1) => 2 (fake (g 1) => 2))
;; 		   (fake (m 1) => 33))]
;;     (expect (rewrite form) => form)))

(deftest wraps-forms-in-expect
  (let [form '( (f 1) => [2]
		(f 2) => (+ 1 2) )
	expected '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> [2])
		    (midje.semi-sweet/expect (f 2) midje.semi-sweet/=> (+ 1 2)))]
    (expect (rewrite form) => expected)))

(deftest handle-provided-clause-test
  (let [form '( (f 1) => [1] :ekey "evalue"
		(f 2) => (+ 2 2)
		(provided (g 3) => 3
			  (g 4) => 4 :pkey "pvalue")
		(f 5) => truthy)
	expected '( (midje.semi-sweet/expect (f 1) midje.semi-sweet/=> [1] :ekey "evalue")
		    (midje.semi-sweet/expect (f 2) midje.semi-sweet/=> (+ 2 2)
					     (midje.semi-sweet/fake (g 3) => 3)
					     (midje.semi-sweet/fake (g 4) => 4 :pkey "pvalue"))
		    (midje.semi-sweet/expect (f 5) midje.semi-sweet/=> truthy))]
    (expect (rewrite form) => expected)))

