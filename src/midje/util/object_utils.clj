(ns ^{:doc "Functions having to do with looking at an object at runtime."}
  midje.util.object-utils)

(defn named-function? [x]
  (:name (meta x)))

(defn object-name [obj]
  (:name (meta obj)))

(defn function-name [funobj]
  (object-name funobj))

(defn name-object [object name]
  (vary-meta object assoc :name name))
