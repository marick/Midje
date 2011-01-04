(ns midje.sweet.t-line-number-insertion-test
  (:use [midje.semi-sweet :only [=> expect]])
  (:use [midje.midje-forms.recognizing :only [loc-is-start-of-arrow-sequence?]])
  (:use [midje.sweet.line-number-insertion])
  (:require [clojure.zip :as zip])
  (:use [clojure.test])
  (:use [midje.test-util]))

(deftest should-be-able-to-deduce-line-numbers
  (let [at-line (fn [line-no form] (with-meta form {:line line-no}))
	assume-position (fn [test-form]
			  (loop [loc (zip/seq-zip test-form)]
			    (if (loc-is-start-of-arrow-sequence? loc)
			      (zip/right loc)
			      (recur (zip/next loc)))))
	finds (fn [form] (arrow-line-number (assume-position form)))]

    ;; most common case
    (expect (finds `( ~(at-line 33 '(f 1)) => 5)) => 33)

    ;; ... but the left-hand-side might be a symbol. We might luck out on right
    ;; ...a... => (exactly 1)
    (expect (finds `( ...a... => ~(at-line 33 '(exactly 1)))) => 33)

    ;; If both, left takes precedence
    (expect (finds `( ~(at-line 33 '(f 1)) => ~(at-line 34 '(exactly 1)))) => 33)

    ;; If neither, look to the left and add one.
    (expect (finds `( (let ~(at-line 32 '[a 2]) a => b))) => 33)
    
    ;; If no line whatsoever can be found, nil
    (expect (finds '( 1 => 2)) => nil)

    ))


(deftest should-be-able-to-add-line-numbers-to-forms
  (let [z (zip/seq-zip '( (f n) => 2  ))
	loc (-> z zip/down zip/right)
	fut add-line-number-to-end-of-arrow-sequence__no-movement
	new-loc (fut 10 loc)]
    (expect (zip/node new-loc) => '=>)
    (expect (zip/root new-loc) => '( (f n) => 2 :file-position (midje.util.file-position/line-number-known 10)))))


(deftest adding-line-number-test
  (let [form `(let ~(with-meta '[a 1] {:line 33})
		a => 2
		~(with-meta '(f 2) {:line 35}) => a)]
    (expect (add-line-numbers form) =>
	    '(clojure.core/let [a 1]
	      midje.sweet.t-line-number-insertion-test/a midje.semi-sweet/=> 2 :file-position (midje.util.file-position/line-number-known 34)
	      (f 2) midje.semi-sweet/=> midje.sweet.t-line-number-insertion-test/a :file-position (midje.util.file-position/line-number-known 35)))))


