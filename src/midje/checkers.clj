(ns midje.checkers)

(defn truthy 
  "Returns precisely true if actual is not nil and not false."
  [actual] 
  (not (not actual)))

(defn falsey 
  "Returns precisely true if actual is nil or false."
  [actual] 
  (not actual))

(defn in-any-order "Produces matcher that matches sequences without regard to order"
  [expected]
  (fn [actual] (= (set expected) (set actual))))
