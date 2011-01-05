;; -*- indent-tabs-mode: nil -*-

(ns midje.util.wrapping
  (:use midje.util.form-utils)
  (:require [clojure.zip :as zip])
  (:require [midje.util.unify :as unify]))

;; TODO: Should this be in with the midje-forms? It's general purpose (except
;; for the use of midje in varnames).

(defn ?form [] (symbol (name (ns-name *ns*)) "?form")) ; this cannot be right

(defn ensure-correct-form-variable [form]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (if (symbol-named? (zip/node loc) "?form")
                         (zip/replace loc (?form))
                         loc))))))

(defn midje-wrapped
  "This is used prevent later wrapping passes from processing the
   code-that-produces-the-value."
  [value] value)

(defn wrapped? [form] (form-first? form "midje-wrapped"))

(defn wrap [outer-form inner-form]
  (unify/subst outer-form {(?form) inner-form}))

(defn multiwrap [form wrappers]
  (if (empty? wrappers)
    `(midje-wrapped ~form)
    (recur (wrap (first wrappers) form)
           (rest wrappers))))

