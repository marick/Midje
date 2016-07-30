(ns midje.checking.checkers.util
  (:require [commons.clojure.core :refer :all :exclude [any?]]
            [midje.util.pile :as pile]))

(defn named-as-call
  "Adds a string name that looks like a function call to
   a function's metadata under :name"
  [name expected function]
  (pile/name-object function (format "(%s %s)" name expected)))
