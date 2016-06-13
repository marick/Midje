(ns behaviors.t-metaconstant-compilation
  (:require [midje.sweet :refer :all]))

;;; Because metaconstants are auto-defined, a file without `metaconstants`
;;; will fail AOT compilation. 

(metaconstants ..m.. ..m.... .mc.)    

(fact "printing" 
  (str .mc.) => ".mc."
  (pr-str .mc.) => ".mc.")

(fact "equality checking"
    (= ..m.. ..m....) => truthy)

(future-fact "Make it so the metaconstant compilation tests are compiled.")
