(ns midje.util.debugging
  (:require [clojure.contrib.string :as str]))

(def indent-count (atom 0))
(def indent (atom ""))

(defn p [& tags]
  (apply (partial println @indent) tags))

(defn p+ [& tags]
  (swap! indent-count inc)
  (swap! indent #(str (str/butlast 1 %) @indent-count ">"))
  (apply p tags))

(defn nopret [val] val)
(defn pret [val]
  (p val)
  (swap! indent-count dec)
  (swap! indent #(str (str/butlast 2 %) ">"))
  val)

