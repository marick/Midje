(ns user.fus-prerequisites--fact-wide
  (:require [midje.sweet :refer :all]))

;; This was bug #161

(unfinished bar)
(defn foo []
  (+ (bar 1) (bar 2)))

(facts
  (prerequisite (bar 1) => 1
                (bar 88) => 88)
  (foo) => 3
  (provided
    (bar 2) => 2))


