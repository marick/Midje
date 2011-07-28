;; -*- indent-tabs-mode: nil -*-

(ns midje.inner-functions)


(defmacro testable-defn [name args & body]
  
)
  
(defn environment-creator [var name]
  (((meta var) :nested) name))

(defmacro within [ [environment-function-symbol & environment-args]
                   [inner-function-symbol & inner-args] ]
  `(((environment-creator (var ~environment-function-symbol) '~inner-function-symbol)
     ~@environment-args) ~@inner-args))
