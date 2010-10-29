(ns leiningen-midje.t-core
  (:use [leiningen-midje.core] :reload)
  (:use [midje.sweet]))



(fact
  (f 1) => 1
  (provided
    (g 1) => 1))
