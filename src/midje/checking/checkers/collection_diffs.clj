;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checking.checkers.collection-diffs
  "Checkers for collections and strings."
  (:use midje.clojure.core)
  (:require [midje.util.pile :as pile]
            [midje.checking.checkers.collection :as old]
            [midje.checking.core :as core]
            flare.map)
  (:import [flare.map MapKeysDiff]))

;; TODO: Currently not handling "looseness"

(defn classify [actual expected] :map:map)

(defmulti contains:diffs (fn [actual expected & _] (classify actual expected)))

(defmethod contains:diffs :map:map [actual expected]
  (let [missing-keys (difference (set (keys expected)) (set (keys actual)))]
    (if (empty? missing-keys)
      []
      [(new MapKeysDiff missing-keys nil)])))
