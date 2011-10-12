;; -*- indent-tabs-mode: nil -*-

(ns midje.util.debugging
  (:require [midje.util.old-clojure-contrib.string :as str]))

;; Typical sequence:
;; (p+ 1)                  > 1
;; (p 2)                   > 2
;; (p+ 3)                  >> 3
;; (pret 4)                >> 4
;; (p 5)                   > 5
;;
;; Add more functions as needed.

(def indent-count (atom 0))
(def indent (atom ""))

(defn p [& tags]
  "Print, indented, with prn."
  (apply (partial prn @indent) tags))

(defn p+ [& tags]
  "Increase the indent level, then print tags, indented, with prn."
  (swap! indent-count inc)
  (swap! indent #(str (str/butlast 1 %) @indent-count ">"))
  (apply p tags))

(defn pret [val]
  "Print the given value at current indent level, then decrease the level"
  (p val)
  (when (> @indent-count 0)
    (swap! indent-count dec)
    (swap! indent #(str (str/butlast 2 %) ">")))
  val)

(defn nopret
  "A no-op. Adding 'no' is easier than deleting a 'pret'."
  [val] val)
