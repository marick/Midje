(ns user.fus-prerequisites--fact-wide
  (:use midje.sweet))

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


(unfinished check-f check-g check-h)
(defn ander [n]
  (and (check-f n) (check-g n) (check-h n)))

(against-background [(check-f 1) => true, (check-g 1) => true, (check-h 1) => true]
   (fact
    (ander 1) => truthy
    (ander 1) => falsey (provided (check-f 1) => false)
    (ander 1) => falsey (provided (check-g 1) => false)
    (ander 1) => falsey (provided (check-h 1) => false)))

