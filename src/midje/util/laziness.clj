(ns midje.util.laziness
  (:require [clojure.zip :as zip]))

;; Code for dealing with Clojure lazy constructs that get in the way when we want
;; to process everything in a lazyseq.

(defn eagerly [value]
  (if (seq? value)
    (loop [loc (zip/seq-zip value)]  ;; touch every node
      (if (zip/end? loc)
	(zip/root loc)
	(recur (zip/next loc))))
    value))

