;; -*- indent-tabs-mode: nil -*-

(ns midje.util.unify
  (:use [clojure.walk :only [prewalk]])
  (:require [clojure.core.unify :as unify]))

(defn- variable? [x] (and (symbol? x) (.startsWith (name x) "?")))

; (def subst unify/subst)
(def unify unify/unify)

(defn bindings-map-or-nil [first-form second-form]
  (try 
    (unify/unify first-form second-form)
    (catch IllegalArgumentException ex nil)))

(defn subst
  "Attempts to substitute the bindings in the appropriate locations in the given expression."
  [x binds]
  (prewalk (fn [expr] 
                  (if (and (variable? expr)
                           (not= (get binds expr 'not-found) 'not-found))
                    (binds expr)
                    expr)) 
                x))

(defn ?form [] (symbol (name (ns-name *ns*)) "?form")) ; this cannot be right

(defn inject-form [outer-form inner-form]
  (subst outer-form {(?form) inner-form}))



