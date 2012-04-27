(ns midje.ideas.t-metaconstant-compilation
  (:use [midje sweet]))

(metaconstants ..m.. ..m.... .mc.)    

(fact "printing" 
  (str .mc.) => ".mc."
  (pr-str .mc.) => ".mc.")

(fact "equality checking"
    (= ..m.. ..m....) => truthy)

(future-fact "Make it so the metaconstant compilation tests are compiled.")
;; Right now, this just serves as a check that the `metaconstants`
;; check works.
