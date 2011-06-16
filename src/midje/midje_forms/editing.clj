(ns midje.midje-forms.editing
  (:use midje.semi-sweet)
  (:use [midje.midje-forms.recognizing :only [is-head-of-form-providing-prerequisites?
					      is-start-of-check-sequence?
					      loc-is-at-full-expect-form?]]
	[midje.midje-forms.moving-around :only [up-to-full-expect-form
						skip-to-rightmost-leaf]]
	[midje.midje-forms.dissecting :only [arrow-form-overrides]]
	[midje.util.file-position :only [line-number-known arrow-line-number]])
  (:require [clojure.zip :as zip]))

(defn- n-times [n zip-fn loc]
  (if (zero? n)
    loc
    (recur (dec n) zip-fn (zip-fn loc))))


(defn remove-moving-right [loc]
  (-> loc zip/remove zip/next)
)

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
  (assert (is-start-of-check-sequence? loc))
  (let [right-hand (-> loc zip/right zip/right)
        arrow-form (-> loc zip/right zip/node)
	additions (arrow-form-overrides (zip/rights right-hand))
        line-number (arrow-line-number (zip/right loc))
	edited-loc (zip/edit loc
			     (fn [loc]
                               (vary-meta 
                                 `(expect ~loc ~arrow-form ~(zip/node right-hand) ~@additions)
                                 assoc :line line-number)))]
    (->> edited-loc
	 zip/right
	 (n-times (+ 1 (count additions)) remove-moving-right)
	 zip/remove)))

(defn add-key-value-to-end-of-arrow-sequence__then__no-movement [key value loc]
  (-> loc
      zip/right
      (zip/insert-right value)
      (zip/insert-right key)
      zip/left))

(defn add-key-value-within-arrow-branch__then__at_arrow [key value loc]
  (->> loc zip/down zip/right zip/right
       (add-key-value-to-end-of-arrow-sequence__then__no-movement key value)))

(defn add-line-number-to-end-of-arrow-sequence__then__no-movement [number loc]
  (add-key-value-to-end-of-arrow-sequence__then__no-movement
   :position `(line-number-known ~number) loc))
