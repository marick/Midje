;; -*- indent-tabs-mode: nil -*-

(ns midje.util.unify
  (:require [me.fogus.unifycle :as unify])
  (:use [clojure.walk :as walk :only [prewalk]]))

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
  (walk/prewalk (fn [expr] 
                  (if (and (variable? expr)
                           (not= (get binds expr 'not-found) 'not-found))
                    (binds expr)
                    expr)) 
                x))
