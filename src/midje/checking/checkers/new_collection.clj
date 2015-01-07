;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checking.checkers.new-collection
  "Checkers for collections and strings."
  (:use midje.clojure.core)
  (:require [midje.util.pile :as pile]
            [midje.checking.checkers.collection :as old]
            [midje.checking.core :as core]))


(defn classify [actual expected] :map:map)

(defmulti impl:contains (fn [actual expected & _] (classify actual expected)))

(defrecord MissingKeysInActualDiff [keys])

(defn- dlf [actual diffs]
  (core/as-data-laden-falsehood {:actual actual :diffs diffs}))

(defmethod impl:contains :map:map [actual expected looseness]
  (let [missing-keys (difference (set (keys expected)) (set (keys actual)))]
    true
    (dlf actual [(new MissingKeysInActualDiff missing-keys)])))

  
(def ; contains
  ^{:midje/checker true
    :arglists '([expected] [expected looseness])}
  contains
  (old/container-checker-maker
   'contains
   (fn [actual expected looseness]
     (apply impl:contains (old/standardized-arguments actual expected looseness)))))
