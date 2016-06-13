(ns user.fus-background
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

(unfinished g)

(defn f [n] (g n))

;;; Background normally doesn't take a let-style list

(background (g 1) => 2)

(fact (f 1) => 2)

;; But it can.

(background [(g 22) => 22])

(fact (f 22) => 22)


