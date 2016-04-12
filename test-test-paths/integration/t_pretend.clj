(ns integration.t-pretend
  (:require [midje.sweet :refer :all]))


(fact "Look, ma! A failure" 1 => 2)
