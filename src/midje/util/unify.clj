(ns ^{:doc "Unification is used in tabular and (against-)background code."}
  midje.util.unify
  (:require [clojure.core.unify :as unify]
            [clojure.walk :refer [prewalk]]))

(def unify unify/unify)

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
