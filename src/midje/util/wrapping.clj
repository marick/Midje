(ns midje.util.wrapping
  (:use midje.util.form-utils)
  (:require [midje.util.unify :as unify]))

(defn ?form [] (symbol (name (ns-name *ns*)) "?form")) ; this cannot be right

(defn midje-wrapped [value] value)
(defn wrapped? [form] (form-first? form "midje-wrapped"))

(defn wrap [outer-form inner-form]
;  (println "wrapping" inner-form "with" outer-form)
  (unify/subst outer-form {(?form) inner-form}))

(defn multiwrap [form wrappers]
  (if (empty? wrappers)
    `(midje-wrapped ~form)
    (multiwrap (wrap (first wrappers) form)
	       (rest wrappers))))

