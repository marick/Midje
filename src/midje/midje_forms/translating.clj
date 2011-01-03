(ns midje.midje-forms.translating
  (:use clojure.contrib.def)
  (:use [clojure.contrib.seq :only [separate]])
  (:use [midje.util thread-safe-var-nesting wrapping form-utils laziness form-utils])
  (:use midje.metaconstants)
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [clojure.zip :as zip])
  (:use [midje.midje-forms building recognizing dissecting])
  (:use [midje.util.debugging]))

(declare midjcoexpand)

;; There are three variants of background forms, here referred to as "wrappers":
;; 1. RAW - wrappers mixed up, like [ (f 1) => 3 (before ...) (f 2) => 3) ]. Needs parsing.
;; 2. CANONICALIZED - one form per wrapper, perhaps some transformation.
;; 3. FINAL - a nesting form that can be unified with included forms.

(defn- canonicalize-raw-wrappers [forms]
  (loop [expanded []
	 in-progress forms]
    (cond (empty? in-progress)
	  expanded

	  (is-arrow-form? in-progress)
	  (let [content (transform/one-fake-body-content in-progress)]
	    (recur (conj expanded (-> content transform/make-fake make-background))
		   (nthnext in-progress (count content))))

	  (seq-headed-by-setup-teardown-form? in-progress)
	  (recur (conj expanded (first in-progress))
		 (rest in-progress))
	  
	  :else
	  (throw (Error. (str "This doesn't look like part of a background: "
			      (vec in-progress)))))))

(defn with-wrapping-target [what target]
  (with-meta what (merge (meta what) {:midje/wrapping-target target})))

(defn for-wrapping-target? [target]
  (fn [actual] (= (:midje/wrapping-target (meta actual)) target)))

(defn- final-state-wrapper [canonicalized-non-fake]
;  (println canonicalized-non-fake)
  (if (some #{(name (first canonicalized-non-fake))} '("before" "after" "around"))
    (with-wrapping-target
      (macroexpand-1 (cons (symbol "midje.midje-forms.building"
				   (name (first canonicalized-non-fake)))
			   (rest canonicalized-non-fake)))
      (second canonicalized-non-fake))
    (throw (Error. (str "Could make nothing of " canonicalized-non-fake)))))

(defn- final-fake-wrapper [fakes]
  (with-wrapping-target
    `(with-pushed-namespace-values :midje/background-fakes ~fakes ~(?form))
    :checks))

;; Collecting all the background fakes is here for historical reasons:
;; it made it easier to eyeball expanded forms and see what was going on.
(defn final-wrappers [raw-wrappers]
  (define-metaconstants raw-wrappers)
  (let [canonicalized (canonicalize-raw-wrappers raw-wrappers)
	[fakes state-wrappers] (separate-by fake? canonicalized)
	final-state-wrappers (eagerly (map final-state-wrapper state-wrappers))]
    (if (empty? fakes)
      final-state-wrappers
      (concat final-state-wrappers (list (final-fake-wrapper fakes))))))

(defmacro- with-additional-wrappers [final-wrappers form]
  `(with-pushed-namespace-values :midje/wrappers ~final-wrappers
    ~form))

(defn replace-wrappers-returning-immediate [raw-wrappers]
  (let [[immediates finals] (separate (for-wrapping-target? :contents)
				      (final-wrappers raw-wrappers))]
    (set-namespace-value :midje/wrappers (list finals))
    (multiwrap "unimportant-value" immediates)))


(defn forms-to-wrap-around [wrapping-target]
  (let [wrappers (namespace-values-inside-out :midje/wrappers)
	per-target-wrappers (filter (for-wrapping-target? wrapping-target) wrappers)]
    per-target-wrappers))

(defn midjcoexpand [form]
  ;; (p+ "== midjcoexpanding" form)
  ;; (p "== with" (namespace-values-inside-out :midje/wrappers))
  (nopret (cond (already-wrapped? form)
	form

	(form-first? form "quote")
	form

	(future-fact? form)
	(macroexpand form)

	(expect? form)
	(multiwrap form (forms-to-wrap-around :checks))

	(fact? form)
	(multiwrap (midjcoexpand (macroexpand form))
		   (forms-to-wrap-around :facts))

	(background-form? form)
	(do
	  ;; (p+ "use these wrappers" (raw-wrappers form))
	  ;; (p "for this form" (interior-forms form))
	  ;; (p (namespace-values-inside-out :midje/wrappers))
	  (nopret (let [wrappers (final-wrappers (raw-wrappers form))
		      [now-wrappers later-wrappers] (separate (for-wrapping-target? :contents)
							      wrappers)]
	    ;; "Now wrappers" have to be separated out and discarded here, because
	    ;; if they were left in, they'd be reapplied in any nested background
	    ;; forms.
	    ;; (p "now-wrappers" now-wrappers)
	    ;; (p "later-wrappers" later-wrappers)
	    (multiwrap (with-additional-wrappers later-wrappers
			  (midjcoexpand (interior-forms form)))
		       now-wrappers))))
	
	(sequential? form)
	(as-type form (eagerly (map midjcoexpand form)))

	:else
	form)))
