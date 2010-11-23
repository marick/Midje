(ns midje.background.midjcoexpand
  (:require [me.fogus.unifycle :as unify])
  (:use [midje.util thread-safe-var-nesting recognizing-forms])
  (:use midje.sweet))

(defn wrap [outer-form inner-form]
  (unify/subst outer-form {'?form inner-form}))

(defn multiwrap [form wrappers]  ; Ha! just because I *can*
  (if (empty? wrappers)
    form
    (multiwrap (wrap (first wrappers) form)
	       (rest wrappers))))

(defn to-be-expanded? [form]
  (form-first? form "fact"))

(defn wrappable? [form]
  (form-first? form "expect"))

(defn midjcoexpand [form]
  (cond (wrappable? form)
	(multiwrap form (namespace-values-inside-out :midje/wrappers))

	(to-be-expanded? form)
	(midjcoexpand (macroexpand form))

	(sequential? form)
	(map midjcoexpand form)
	
	:else
	form))

