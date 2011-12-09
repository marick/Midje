;; -*- indent-tabs-mode: nil -*-

(ns midje.util.unify
  (:use [clojure.walk :only [prewalk]])
  (:require [clojure.core.unify :as unify]))

(def unify unify/unify)

(defn bindings-map-or-nil [first-form second-form]
  (try 
    (unify/unify first-form second-form)
    (catch IllegalArgumentException ex nil)))

(defn substitute
  "Attempts to substitute the bindings into any symbol in the given form."
  [form bindings]
  (prewalk (fn [expr] 
                (if (and (symbol? expr)
                         (contains? bindings expr))
                  (bindings expr)
                  expr)) 
                form))

(defn ?form [] (symbol (name (ns-name *ns*)) "?form")) ; this cannot be right

(defn inject-form [outer-form inner-form]
  (substitute outer-form {(?form) inner-form}))