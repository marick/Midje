(ns midje.parsing.util.fnref
  "A fnref is the first symbol-or-var in a list. These utilities
   allow you to interpret it in multiple ways."
  (:require [such.control-flow :refer [branch-on]]))

(defn classify-function-reference [reference]
  (branch-on reference
     symbol?        :symbol
     sequential?    :var-form
     :else          (throw (Exception. "Programmer error"))))

(defmulti as-symbol classify-function-reference)
(defmethod as-symbol :symbol [reference]
  reference)
(defmethod as-symbol :var-form [reference]
  (second reference))

(defmulti as-var-form classify-function-reference)
(defmethod as-var-form :symbol [reference]
  `(var ~reference))
(defmethod as-var-form :var-form [reference]
  reference)

(defmulti as-form-to-fetch-var-value classify-function-reference)
(defmethod as-form-to-fetch-var-value :symbol [reference]
  reference)
(defmethod as-form-to-fetch-var-value :var-form [reference]
  `(deref ~reference))

;; Unlike other functions, this doesn't return homoiconic forms to
;; substitute into macroexpansions. Instead, it returns the actual
;; clojure.lang.Var object.
(defmulti resolved-to-actual-var-object classify-function-reference)
(defmethod resolved-to-actual-var-object :symbol [reference]
  (resolve reference))
(defmethod resolved-to-actual-var-object :var-form [reference]
  (resolved-to-actual-var-object (second reference)))
