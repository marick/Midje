;; -*- indent-tabs-mode: nil -*-

(ns midje.util.strings)

(defn newlineify [strings]
  (map #(str % (System/getProperty "line.separator")) strings))

(defn prefix [strings with]
  (map #(str with %) strings))



