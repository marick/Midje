(ns midje.midje-forms.translating
  (:use clojure.contrib.def)
  (:use [midje.util thread-safe-var-nesting wrapping form-utils laziness form-utils])
  (:use midje.sweet.metaconstants)
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
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


(defn- make-final [canonicalized-non-fake]
  (let [bindings (setup-teardown-bindings canonicalized-non-fake)]
    ;; (println "== Makefinal for " canonicalized-non-fake)
    ;; (println bindings)
    ;; (println (bindings '?form))
    ;; (println "key" (bindings '?key))
    ;; (println "test" (= (name (bindings '?key)) "before"))
    (cond (= (name (bindings '?key)) "before")
	  (do ; (println "before")
	      `(try ; (println "BEFORE: " '~(bindings '?form))
		    ~(bindings '?form)
		    ~(?form)
   	       (finally ; (println "AFTER:" '~(bindings '?teardown))
			~(bindings '?teardown))))
	  
	  (= (name (bindings '?key)) "after")	  
	  (do ; (println "after")
	      `(try  ~(?form) (finally ~(bindings '?form))))

	  :else
	  (throw (Error. (str "Could make nothing of " canonicalized-non-fake))))))

;; Collecting all the background fakes is here for historical reasons:
;; it made it easier to eyeball expanded forms and see what was going on.
(defn- final-wrappers [raw-wrappers]
  (define-metaconstants raw-wrappers)
  (let [canonicalized (canonicalize-raw-wrappers raw-wrappers)
	[fakes others] (separate-by fake? canonicalized)]
    ;; (println "the binding map" (map make-final others)) 
    `[    ~@(eagerly (map make-final others))
      (with-pushed-namespace-values :midje/background-fakes ~fakes ~(?form)) ]))

(defmacro- with-additional-wrappers [raw-wrappers form]
  `(with-pushed-namespace-values :midje/wrappers (final-wrappers ~raw-wrappers)
    ~form))

(defn replace-wrappers [raw-wrappers]
  (set-namespace-value :midje/wrappers (list (final-wrappers raw-wrappers))))

(defn midjcoexpand [form]
;   (println "== midjcoexpanding" form)
;   (println "== with" (namespace-values-inside-out :midje/wrappers))
  (nopret (cond (already-wrapped? form)
	form

	(form-first? form "quote")
	form

	(wrappable? form)
	(multiwrap form (namespace-values-inside-out :midje/wrappers))

	(expansion-has-wrappables? form)
	(midjcoexpand (macroexpand form))

	(provides-wrappers? form)
	(do
;;	  (println "use these wrappers" (raw-wrappers form))
;;	  (println "for this form" (interior-forms form))
	  (with-additional-wrappers (raw-wrappers form)
	    (midjcoexpand (interior-forms form))))
	
	(sequential? form)
	(as-type form (eagerly (map midjcoexpand form)))

	:else
	form)))

