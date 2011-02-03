;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-deprecation
  (:use [midje sweet test-util]))

(unfinished g)
(defn f [a] (g a))

(let [messages (with-out-str 
                 (fact
                   (f 1) => 1
                   (provided (g odd?) => 1)))]
  (facts
    messages => (contains "a function named 'odd?'")
    messages => (contains "(t_deprecation.clj:11")))

(let [messages (with-out-str 
                 (fact
                   (f 1) => 1
                   (provided (g (as-checker odd?)) => 1)

                   (f 1) => 1
                   (provided (g (checker [n] true)) => 1)

                   (f 1) => 1
                   (provided (g (exactly 1)) => 1)
  
                   (f even?) => 1
                   (provided (g (exactly even?)) => 1)))]
  ((contains "WARNING") messages) => falsey)
