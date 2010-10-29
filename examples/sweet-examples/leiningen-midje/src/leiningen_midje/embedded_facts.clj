(ns leiningen-midje.embedded-facts
  [:use [midje.sweet]])


(defn embedded [n] 1)


(facts
 (embedded 1) => 1
 (embedded 2) => 2)
