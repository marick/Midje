(ns midje.util.unify
  (:require [me.fogus.unifycle :as unify]))

(def subst unify/subst)
(def unify unify/unify)

(defn bindings-map-or-nil [first-form second-form]
  (try 
    (unify/unify first-form second-form)
    (catch IllegalArgumentException ex nil)))

