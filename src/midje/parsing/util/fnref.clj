(ns ^{:doc ""}
  midje.parsing.util.fnref
  (:use [midje.util.form-utils])
  )


(defn classify-function-reference [reference]
  (pred-cond reference
     symbol?        :symbol
     sequential?    :var-form
     :else          (throw (Exception. "Programmer error"))))

(defmulti fnref-symbol classify-function-reference)
(defmethod fnref-symbol :symbol [reference]
  reference)
(defmethod fnref-symbol :var-form [reference]
  (second reference))

(defmulti fnref-call-form classify-function-reference)
(defmethod fnref-call-form :symbol [reference]
  `(var ~reference))
(defmethod fnref-call-form :var-form [reference]
  reference)
  
(defmulti fnref-dereference-form classify-function-reference)
(defmethod fnref-dereference-form :symbol [reference]
  reference)
(defmethod fnref-dereference-form :var-form [reference]
  `(deref ~reference))

;; Unlike other functions, this doesn't return homoiconic forms to
;; substitute into macroexpansions. Instead, it returns the actual
;; clojure.lang.Var object.
(defmulti fnref-var-object classify-function-reference)
(defmethod fnref-var-object :symbol [reference]
  (resolve reference))
(defmethod fnref-var-object :var-form [reference]
  reference)
  
