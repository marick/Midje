(ns midje.checkers)

(defn #^{:doc "Returns precisely true if arg is not nil and not false."}
  truthy [arg] (not (not arg)))

(defn #^{:doc "Returns precisely true if arg is nil or false."}
  falsey [arg] (not arg))
