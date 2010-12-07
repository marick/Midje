(ns midje.midje-forms.translating
  (:use clojure.contrib.def)
  (:use [midje.util thread-safe-var-nesting wrapping form-utils laziness form-utils])
  (:use midje.sweet.metaconstants)
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
	  (throw (Error. (str "This doesn't look like part of a background: %s"
			      (vec in-progress)))))))

(defn- replace-with-magic-form [form]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (if (symbol-named? (zip/node loc) "?form")
			 (zip/replace loc (?form))
			 loc))))))

(defmacro before [when before-form & extras ]
  (let [after-form (second extras)]
    `(try
       ~before-form
       ~(?form)
       (finally ~after-form))))

(defmacro after [when after-form]
  `(try ~(?form) (finally ~after-form)))

(defmacro around [when around-form]
  (replace-with-magic-form around-form))

(defn- make-final [canonicalized-non-fake]
;  (println canonicalized-non-fake)
  (if (some #{(name (first canonicalized-non-fake))} '("before" "after" "around"))
    { :when (second canonicalized-non-fake)
      :what (macroexpand-1 (cons (symbol "midje.midje-forms.translating"
					 (name (first canonicalized-non-fake)))
				 (rest canonicalized-non-fake)))
    }
    (throw (Error. (str "Could make nothing of " canonicalized-non-fake)))))

;; Collecting all the background fakes is here for historical reasons:
;; it made it easier to eyeball expanded forms and see what was going on.
(defn- final-wrappers [raw-wrappers]
  (define-metaconstants raw-wrappers)
  (let [canonicalized (canonicalize-raw-wrappers raw-wrappers)
	[fakes others] (separate-by fake? canonicalized)]
    ;;    (println "the binding map" (map make-final others)) 
    `[    ~@(eagerly (map make-final others))
      { :when :checks
       :what (with-pushed-namespace-values :midje/background-fakes ~fakes ~(?form))
       }]))

(defmacro- with-additional-wrappers [raw-wrappers form]
  `(with-pushed-namespace-values :midje/wrappers (final-wrappers ~raw-wrappers)
    ~form))

(defn replace-wrappers [raw-wrappers]
  (set-namespace-value :midje/wrappers (list (final-wrappers raw-wrappers))))

(defn forms-to-wrap-around-each-check []
  (let [wrapper-maps (namespace-values-inside-out :midje/wrappers)
	per-check-wrapper-maps (filter #(= (:when %) :checks) wrapper-maps)
	wrappers (map :what per-check-wrapper-maps)]
    wrappers))
	

(defn midjcoexpand [form]
;   (println "== midjcoexpanding" form)
;   (println "== with" (namespace-values-inside-out :midje/wrappers))
  (nopret (cond (already-wrapped? form)
	form

	(form-first? form "quote")
	form

	(check-wrappable? form)
	(multiwrap form (forms-to-wrap-around-each-check))

	(expansion-has-wrappables? form)
	(midjcoexpand (macroexpand form))

	(provides-wrappers? form)
	(do
;;	  (println "use these wrappers" (raw-wrappers form))
;;	  (println "for this form" (interior-forms form))
;;	  (println (namespace-values-inside-out :midje/wrappers))
	  (with-additional-wrappers (raw-wrappers form)
	      (midjcoexpand (interior-forms form))))
	
	(sequential? form)
	(as-type form (eagerly (map midjcoexpand form)))

	:else
	form)))

