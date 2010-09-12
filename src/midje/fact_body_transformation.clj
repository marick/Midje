(ns midje.fact-body-transformation
  (:use clojure.test
	midje.semi-sweet
        [clojure.contrib.ns-utils :only [immigrate]])
  (:require [clojure.zip :as zip])
)

;; Identifying forms

;; TODO: There must be a better way of handling namespaces.
(defn namespacey-match [symbols loc]
  (let [base-names (map name symbols)
	qualified-names (map #(str "midje.semi-sweet/" %) base-names)]
    ( (set (concat base-names qualified-names)) (str (zip/node loc)))))

(defn is-semi-sweet-keyword? [loc]
  (namespacey-match '(expect fake) loc))

(defn head-of-provided-form? [loc]
  (namespacey-match '(provided) loc))

(defn at-full-expect-form? [loc]
  (and (zip/branch? loc)
       (namespacey-match '(expect) (zip/down loc))))

(defn start-of-arrow-sequence? [loc]
  (and (zip/right loc)
       (namespacey-match '(=>) (zip/right loc))))

(defn overrides [loc]
  (apply concat (take-while #(keyword? (first %)) (partition 2 (zip/rights loc)))))

;; Simple movement

(defn up-to-full-expect-form [loc]
  (if (at-full-expect-form? loc)
    loc
    (recur (zip/up loc))))

(defn skip-to-end-of-full-form [loc]
  (let [end-form (zip/rightmost loc)]
    (if (zip/branch? end-form)
      (recur (zip/down end-form))
      end-form)))

;; Editing

(defn remove-n [n loc]
  (if (= n 0)
    loc
    (recur (- n 1) (zip/remove loc))))

(defn remove-n [loc n]
  (if (= n 0)
    loc
    (recur (zip/remove loc) (- n 1))))

(defn delete-enclosing-provided-form__at-previous-full-expect-form [loc]
  (assert (head-of-provided-form? loc))
  (let [x (-> loc zip/up zip/remove)]
    (up-to-full-expect-form x)))

(defn tack-on__at-same-location [forms loc]
  (assert (at-full-expect-form? loc))
  (if (empty? forms)
    (up-to-full-expect-form loc)
    (recur (rest forms) (zip/append-child loc (first forms)))))

(defn tack-on__at-rightmost-expect-location [forms loc]
  (let [tack (fn [loc] (tack-on__at-same-location forms loc))]
    (-> loc tack zip/down skip-to-end-of-full-form)))

(defn wrap-with-expect__at-rightmost-wrapped-location [loc]
  (assert (start-of-arrow-sequence? loc))
  (let [right-hand (-> loc zip/right zip/right)
	additions (overrides right-hand)
	edited-loc (zip/edit loc
			     (fn [loc] `(expect ~loc => ~(zip/node right-hand))))]
    (-> edited-loc zip/right zip/right (remove-n 2))))

;; The meat of it.

(defn expand-following-into-fake-calls [provided-loc]
  (let [expectations (rest (zip/node (zip/up provided-loc)))
	triplets (vec (partition 3 expectations))]
    (map (fn [triplet] `(midje.semi-sweet/fake ~@triplet))
	 triplets)))

(defn rewrite [multi-form]
  (loop [loc (zip/seq-zip multi-form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond (start-of-arrow-sequence? loc)
			     (wrap-with-expect__at-rightmost-wrapped-location loc)

			     (head-of-provided-form? loc)
			     (let [fake-calls (expand-following-into-fake-calls loc)
				   full-expect-form (delete-enclosing-provided-form__at-previous-full-expect-form loc)]
			       (tack-on__at-rightmost-expect-location fake-calls full-expect-form))

			     (is-semi-sweet-keyword? loc)
			     (throw (Exception. (str "Why is this form processed twice?" (zip/node loc))))
			     ; See comment in ignores-semi-sweet-constructs-test
;			     (skip-to-end-of-full-form loc)

			     :else loc))))))
	
