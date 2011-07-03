(ns midje.midje-forms.editing
  (:use midje.semi-sweet)
  (:use [midje.arrows :only [is-start-of-arrow-sequence?
                             arrow-sequence-overrides]])
  (:use [midje.midje-forms.recognizing :only [is-head-of-form-providing-prerequisites?
					      loc-is-at-full-expect-form?]]
	[midje.midje-forms.moving-around :only [up-to-full-expect-form
						skip-to-rightmost-leaf]]
	[midje.util.file-position :only [line-number-known arrow-line-number]])
  (:require [clojure.zip :as zip]))

(defn- n-times [n zip-fn loc]
  (if (zero? n)
    loc
    (recur (dec n) zip-fn (zip-fn loc))))


(defn remove-moving-right [loc]
  (-> loc zip/remove zip/next))

(defn delete_prerequisite_form__then__at-previous-full-expect-form [loc]
  (assert (is-head-of-form-providing-prerequisites? loc))
  (let [x (-> loc zip/up zip/remove)]
    (up-to-full-expect-form x)))

(defn tack-on__then__at-same-location [forms loc]
  (assert (loc-is-at-full-expect-form? loc))
  (if (empty? forms)
    (up-to-full-expect-form loc)
    (recur (rest forms) (zip/append-child loc (first forms)))))

(defn tack-on__then__at-rightmost-expect-leaf [forms loc]
  (let [tack (fn [loc] (tack-on__then__at-same-location forms loc))]
    (-> loc tack zip/down skip-to-rightmost-leaf)))

(defn wrap-with-expect__then__at-rightmost-expect-leaf [loc]
  (assert (is-start-of-arrow-sequence? loc))
  (let [right-hand (-> loc zip/right zip/right)
        arrow-sequence (-> loc zip/right zip/node)
	additions (arrow-sequence-overrides (zip/rights right-hand))
        line-number (arrow-line-number (zip/right loc))
	edited-loc (zip/edit loc
			     (fn [loc]
                               (vary-meta 
                                 `(expect ~loc ~arrow-sequence ~(zip/node right-hand) ~@additions)
                                 assoc :line line-number)))]
    (->> edited-loc
	 zip/right
	 (n-times (+ 1 (count additions)) remove-moving-right)
	 zip/remove)))

