(ns midje.inner-functions)


(defmacro testable-defn [_name_ _args_ & _body_]
  
)
  
(defn environment-creator [var name]
  (((meta var) :nested) name))

(defmacro within [ [environment-function-symbol & environment-args]
                   [inner-function-symbol & inner-args] ]
  `(((environment-creator (var ~environment-function-symbol) '~inner-function-symbol)
     ~@environment-args) ~@inner-args))
