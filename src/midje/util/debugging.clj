;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Functions for printing indented output for use in debugging."}
  midje.util.debugging
  (:use [utilize.string :only (but-last-str)]))

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

(defn p
  "Print, indented, with prn."
  [& tags]
  (apply (partial prn @indent) tags))

(defn p+
  "Increase the indent level, then print tags, indented, with prn."
  [& tags]
  (swap! indent-count inc)
  (swap! indent #(str (but-last-str 1 %) @indent-count ">"))
  (apply p tags))

(defn pret
  "Print the given value at current indent level, then decrease the level"
  [val]
  (p val)
  (when (> @indent-count 0)
    (swap! indent-count dec)
    (swap! indent #(str (but-last-str 2 %) ">")))
  val)

(defn nopret
  "A no-op. Adding 'no' is easier than deleting a 'pret'."
  [val] 
  val)