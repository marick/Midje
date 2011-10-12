;; -*- indent-tabs-mode: nil -*-

;;; Functions having to do with looking at an object at runtime.

(ns midje.util.object-utils)

(defn named-function? [thing]
  (:name (meta thing)))

(defn object-name [obj]
  (:name (meta obj)))

(defn function-name [funobj]
  (object-name funobj))

(defn function-name-or-spewage [funobj]
  (or (:name (meta funobj)) (pr-str funobj)))

(defn name-object [object name]
  (vary-meta object assoc :name name))
