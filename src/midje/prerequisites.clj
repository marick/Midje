(ns midje.prerequisites
  (:use [midje.util.namespace :only [namespacey-match]]
  ))

(defn is-head-of-form-providing-prerequisites? [loc]
  (namespacey-match '(provided) loc))
